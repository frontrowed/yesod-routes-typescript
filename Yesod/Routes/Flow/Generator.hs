{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Routes.Flow.Generator
    ( genFlowRoutesPrefix
    , genFlowRoutes
    ) where

import ClassyPrelude hiding (FilePath)
import Data.Text (dropWhileEnd)
import Filesystem (createTree, writeTextFile)
import Filesystem.Path (FilePath, directory)
import Yesod.Routes.TH.Types
import qualified Data.Char as C
import qualified Data.Map  as M
import qualified Data.Text as T


genFlowRoutes :: [ResourceTree String] -> FilePath -> IO ()
genFlowRoutes ra fp = genFlowRoutesPrefix [] [] ra fp "''"

genFlowRoutesPrefix :: [String] -> [String] -> [ResourceTree String] -> FilePath -> Text -> IO ()
genFlowRoutesPrefix routePrefixes elidedPrefixes fullTree fp prefix = do
    createTree $ directory fp
    writeTextFile fp routesCs
  where
    routesCs =
      let classes =
            map disambiguateFields $
            resourceTreeToClasses elidedPrefixes $
            ResourceParent "paths" False [] [] hackedTree
      in    "/* @flow */\n\n"
         <> classesToFlow classes
         <> "\n\nvar PATHS: PATHS_TYPE_paths = new PATHS_TYPE_paths(" <> prefix <> ");\n"

    -- Route hackery.
    landingRoutes = flip filter fullTree $ \case
        ResourceParent _ _ _ _ _ -> False
        ResourceLeaf res -> not $ elem (resourceName res) ["AuthR", "StaticR"]
    parents =
        -- if routePrefixes is empty, include all routes
        filter (\n -> null routePrefixes || any (parentName n) routePrefixes) fullTree
    hackedTree = ResourceParent "staticPages" False [] [] landingRoutes : parents

parentName :: ResourceTree String -> String -> Bool
parentName (ResourceParent n _ _ _ _) name = n == name
parentName _ _  = False

----------------------------------------------------------------------

data RenderedPiece =
    Path Text
  | Number
  | String

isVariable :: RenderedPiece -> Bool
isVariable (Path _) = False
isVariable _        = True

renderRoutePieces :: [Piece String] -> [RenderedPiece]
renderRoutePieces = map renderRoutePiece
  where
    renderRoutePiece (Static st)   = Path $ T.dropAround (== '/') $ pack st
    renderRoutePiece (Dynamic typ) = if isNumberType typ then Number else String

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
      { cmField     :: Text            -- ^ Field name used to refer to the child class.
      , cmClassName :: Text            -- ^ Class name of the child class.
      }
    -- | A callable method.
  | Method
      { cmField     :: Text            -- ^ Field name used to refer to the method.
      , cmPieces    :: [RenderedPiece] -- ^ Pieces to render the route.
      }

variableCount :: ClassMember -> Int
variableCount ChildClass {} = 0
variableCount Method {..}   = length (filter isVariable cmPieces)

variableNames :: [Text]
variableNames = T.cons <$> ['a'..'z'] <*> ("" : variableNames)

----------------------------------------------------------------------

-- | Create a list of 'Class'es from a 'ResourceTree'.
resourceTreeToClasses :: [String] -> ResourceTree String -> [Class]
resourceTreeToClasses elidedPrefixes = finish . go Nothing []
  where
    finish (Right (_, classes)) = classes
    finish (Left _)             = []

    go :: Maybe Text -> [RenderedPiece] -> ResourceTree String -> Either (Maybe ClassMember) ([ClassMember], [Class])
    go _parent routePrefix (ResourceLeaf res) =
      Left $ do
        Methods _ methods <- return $ resourceDispatch res -- Ignore subsites.
        guard (not $ null methods) -- Silently ignore routes without methods.
        let resName  = T.replace "." "" $ T.replace "-" "_" fullName
            fullName = intercalate "_" [pack st :: Text | Static st <- resourcePieces res]
        return Method
          { cmField       = if null fullName then "_" else resName
          , cmPieces      = routePrefix <> renderRoutePieces (resourcePieces res) }
    go parent routePrefix (ResourceParent name _ pieces _queries children) =
      let elideThisPrefix = name `elem` elidedPrefixes
          pref            = cleanName $ pack name
          jsName          = maybe "" (<> "_") parent <> pref
          newParent       = if elideThisPrefix then parent else Just jsName
          newRoutePrefix  = routePrefix <> renderRoutePieces pieces
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
cleanName = underscorize . uncapitalize . dropWhileEnd C.isUpper
  where uncapitalize t = (toLower $ take 1 t) <> drop 1 t
        underscorize = T.pack . go . T.unpack
          where go (c:cs) | C.isUpper c = '_' : C.toLower c : go cs
                          | otherwise   =  c                : go cs
                go [] = []

----------------------------------------------------------------------

-- | Disambiguate fields by appending suffixes.
disambiguateFields :: Class -> Class
disambiguateFields klass = klass { classMembers = processMembers $ classMembers klass }
  where
    processMembers = fromMap . disambiguate viaLetters . disambiguate viaArgCount . toMap
    fromMap  = concat . M.elems
    toMap    = M.fromListWith (++) . labelled
    labelled = map (cmField &&& return)
    append t = \cm -> cm { cmField = cmField cm <> t cm }

    disambiguate :: ([ClassMember] -> [ClassMember]) -> M.Map Text [ClassMember] -> M.Map Text [ClassMember]
    disambiguate inner = M.fromListWith (++) . concatMap f . M.toList
      where
        f :: (Text, [ClassMember]) -> [(Text, [ClassMember])]
        f y@(_, [ ]) = [y]
        f y@(_, [_]) = [y]
        f   (_, xs ) = labelled $ inner xs

    -- Append the number of arguments.
    viaArgCount = map $ append (T.pack . show . variableCount)

    -- Append arbitrary letters as a last resort.
    viaLetters  = zipWith (append . const) variableNames

----------------------------------------------------------------------

classMemberToFlowDef :: ClassMember -> Text
classMemberToFlowDef ChildClass {..} = "  " <> cmField <> " : " <> cmClassName <> ";\n"
classMemberToFlowDef Method {..}     = "  " <> cmField <> "(" <> args <> "): string { " <> body <> "; }\n"
  where
    args = intercalate ", " $ zipWith render variableNames (filter isVariable cmPieces)
      where
        render name typ = name <> ": " <> (case typ of { Number -> "number"; String -> "string" })

    body = "return this.root + '" <> routeStr variableNames cmPieces <> "'"
      where
        routeStr vars (Path p:rest) = (if null p then "" else "/" <> p) <> routeStr vars rest
        routeStr (v:vars)  (_:rest) = "/' + " <> v <> ".toString() + '" <> routeStr vars rest
        routeStr _         _        = ""

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
