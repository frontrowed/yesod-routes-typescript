{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Routes.Flow.Generator
    ( genFlowRoutesPrefix
    , genFlowRoutes
    ) where

import ClassyPrelude hiding (FilePath)
import Data.Text (dropWhileEnd)
import qualified Data.Text as DT
import Filesystem (createTree, writeTextFile)
import Filesystem.Path (FilePath, directory)
import qualified Data.Char as DC
import Yesod.Routes.TH.Types
    -- ( ResourceTree(..),
    --   Piece(Dynamic, Static),
    --   FlatResource,
    --   Resource(resourceDispatch, resourceName, resourcePieces),
    --   Dispatch(Methods, Subsite) )


genFlowRoutes :: [ResourceTree String] -> FilePath -> IO ()
genFlowRoutes ra fp = genFlowRoutesPrefix [] [] ra fp "''"

genFlowRoutesPrefix :: [String] -> [String] -> [ResourceTree String] -> FilePath -> Text -> IO ()
genFlowRoutesPrefix routePrefixes elidedPrefixes fullTree fp prefix = do
    createTree $ directory fp
    writeTextFile fp routesCs
  where
    routesCs =
      let classes =
            resourceTreeToClasses elidedPrefixes $
            ResourceParent "paths" False [] hackedTree
      in    "/* @flow */\n\n"
         <> classesToFlow classes
         <> "\n\nvar PATHS: PATHS_TYPE_paths = new PATHS_TYPE_paths(" <> prefix <> ");\n"

    -- Route hackery.
    landingRoutes = flip filter fullTree $ \case
        ResourceParent _ _ _ _ -> False
        ResourceLeaf res -> not $ elem (resourceName res) ["AuthR", "StaticR"]
    parents =
        -- if routePrefixes is empty, include all routes
        filter (\n -> null routePrefixes || any (parentName n) routePrefixes) fullTree
    hackedTree = ResourceParent "staticPages" False [] landingRoutes : parents

parentName :: ResourceTree String -> String -> Bool
parentName (ResourceParent n _ _ _) name = n == name
parentName _ _  = False

renderRoutePieces :: [Piece String] -> Text
renderRoutePieces pieces = intercalate "/" $ map renderRoutePiece pieces
  where
    renderRoutePiece p = case p of
        Static st                          -> pack st :: Text
        Dynamic type_ | isNumberType type_ -> ": number"
                      | otherwise          -> ": string"

    isNumberType "Int" = True
    isNumberType type_ = "Id" `isSuffixOf` type_ -- UserId, PageId, PostId, etc.

----------------------------------------------------------------------

-- | A Flow class that will be generated.
data Class =
  Class
    { className    :: Text
    , classMembers :: [ClassMember]
    }

data ClassMember =
    -- | A 'ResourceParent' inside the 'ResourceParent'
    -- that generated this class.
    ChildClass
      { cmClassName   :: Text           -- ^ Class name of the child class.
      , cmField       :: Text           -- ^ Field name used to refer to the child class.
      }
    -- | A callable method.
  | Method
      { cmField       :: Text           -- ^ Field name used to refer to the method.
      , cmPieces      :: [Piece String] -- ^ Pieces, used for arguments and the method body.
      , cmRoutePrefix :: Text           -- ^ Route prefix leading to this method.
      }

----------------------------------------------------------------------

-- | Create a list of 'Class'es from a 'ResourceTree'.
resourceTreeToClasses :: [String] -> ResourceTree String -> [Class]
resourceTreeToClasses elidedPrefixes = finish . go Nothing ""
  where
    finish (Right (_, classes)) = classes
    finish (Left _)             = []

    go :: Maybe Text -> Text -> ResourceTree String -> Either (Maybe ClassMember) ([ClassMember], [Class])
    go _parent routePrefix (ResourceLeaf res) =
      Left $ do
        Methods _ methods <- return $ resourceDispatch res -- Ignore subsites.
        guard (not $ null methods) -- Silently ignore routes without methods.
        let resName  = DT.replace "." "" $ DT.replace "-" "_" fullName
            fullName = intercalate "_" [pack st :: Text | Static st <- resourcePieces res]
        return Method
          { cmField       = if null fullName then "_" else resName
          , cmPieces      = resourcePieces res
          , cmRoutePrefix = routePrefix }
    go parent routePrefix (ResourceParent name _ pieces children) =
      let elideThisPrefix = name `elem` elidedPrefixes
          pref            = cleanName $ pack name
          jsName          = maybe "" (<> "_") parent <> pref
          newParent       = if elideThisPrefix then parent else Just jsName
          newRoutePrefix  = routePrefix <> "/" <> renderRoutePieces pieces <> "/"
          membersMethods  = catMaybes childrenMethods
          (childrenMethods, childrenClasses) = partitionEithers $ map (go newParent newRoutePrefix) children
          (membersClasses, moreClasses)      = concat *** concat $ unzip childrenClasses
      in Right $
           if elideThisPrefix
           then (membersClasses, moreClasses)
           else
             let ourClass =
                   Class
                     { className    = "PATHS_TYPE_" <> jsName
                     , classMembers = membersClasses ++ membersMethods }
                 ourReference =
                   ChildClass
                     { cmClassName  = className ourClass
                     , cmField      = pref }
             in ([ourReference], ourClass : moreClasses)

cleanName :: Text -> Text
cleanName = underscorize . uncapitalize . dropWhileEnd DC.isUpper
  where uncapitalize t = (toLower $ take 1 t) <> drop 1 t
        underscorize = DT.pack . go . DT.unpack
          where go (c:cs) | DC.isUpper c = '_' : DC.toLower c : go cs
                          | otherwise    =  c                 : go cs
                go [] = []

----------------------------------------------------------------------

classMemberToFlowDef :: ClassMember -> Text
classMemberToFlowDef ChildClass {..} = "  " <> cmField <> " : " <> cmClassName <> ";\n"
classMemberToFlowDef Method {..}     = "  " <> cmField <> "(" <> args <> "): string { " <> body <> "; }\n"
  where
    args = intercalate ", " $ map (uncurry (<>)) variables

    body = "return this.root + '" <> routeStr variables variablePieces <> "'"
      where
        variablePieces = map (\p -> if isVariable p then Right p else Left p) pieces
        routeStr vars ((Left p):rest) | null p    = routeStr vars rest
                                      | otherwise = "/" <> p <> routeStr vars rest
        routeStr (v:vars) ((Right _):rest) = "/' + " <> fst v <> ".toString() + '" <> routeStr vars rest
        routeStr [] [] = ""
        routeStr _  [] = error "extra vars!"
        routeStr [] _  = error "no more vars!"

    routeString  = DT.replace "//" "/" cmRoutePrefix <> renderRoutePieces cmPieces
    pieces       = DT.splitOn "/" routeString
    isVariable r = length r > 1 && DT.head r == ':'
    variables    = zip variableNames (filter isVariable pieces)
      where variableNames = DT.cons <$> ['a'..'z'] <*> ("" : variableNames)

classMemberToFlowInit :: ClassMember -> Text
classMemberToFlowInit ChildClass {..} = "    this." <> cmField <> " = new " <> cmClassName <> "(root);\n"
classMemberToFlowInit Method {}       = ""

classToFlow :: Class -> Text
classToFlow Class {..} =
    "class " <> className <> " {\n"
  <> concat (classMemberToFlowDef <$> classMembers)
  <> "\n"
  <> "  root: string;\n"
  <> "  constructor(root: string) {\n"
  <> "    this.root = root;\n"
  <> concat (classMemberToFlowInit <$> classMembers)
  <> "  }\n"
  <> "}\n"

classesToFlow :: [Class] -> Text
classesToFlow = intercalate "\n" . map classToFlow

deriving instance (Show a) => Show (ResourceTree a)
deriving instance (Show a) => Show (FlatResource a)
