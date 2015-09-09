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
genFlowRoutesPrefix routePrefixes elidedPrefixes resourcesApp fp prefix = do
    createTree $ directory fp
    writeTextFile fp routesCs
  where
    routesCs =
        let res = (resToCoffeeString elidedPrefixes Nothing "" $ ResourceParent "paths" False [] hackedTree)
        in  "/* @flow */\n" <>
            either id id (snd res)
            <> "\nvar PATHS: PATHS_TYPE_paths = new PATHS_TYPE_paths("<>prefix<>");\n"

    -- route hackery..
    fullTree = resourcesApp :: [ResourceTree String]
    landingRoutes = flip filter fullTree $ \case
        ResourceParent _ _ _ _ -> False
        ResourceLeaf res -> not $ elem (resourceName res) ["AuthR", "StaticR"]

    parents =
        -- if routePrefixes is empty, include all routes
        filter (\n -> routePrefixes == [] || any (parentName n) routePrefixes) fullTree
    hackedTree = ResourceParent "staticPages" False [] landingRoutes : parents

cleanName :: Text -> Text
cleanName = underscorize . uncapitalize . dropWhileEnd DC.isUpper
  where uncapitalize t = (toLower $ take 1 t) <> drop 1 t
        underscorize = DT.pack . go . DT.unpack
          where go (c:cs) | DC.isUpper c = '_' : DC.toLower c : go cs
                          | otherwise    =  c                 : go cs
                go [] = []

isNumberType :: String -> Bool
isNumberType "Int" = True
isNumberType type_ = "Id" `isSuffixOf` type_ -- UserId, PageId, PostId, etc.

renderRoutePieces :: [Piece String] -> Text
renderRoutePieces pieces = intercalate "/" $ map renderRoutePiece pieces
  where
    renderRoutePiece p = case p of
        Static st                          -> pack st :: Text
        Dynamic type_ | isNumberType type_ -> ": number"
                      | otherwise          -> ": string"

parentName :: ResourceTree String -> String -> Bool
parentName (ResourceParent n _ _ _) name = n == name
parentName _ _  = False

resToCoffeeString :: [String] -> Maybe Text -> Text -> ResourceTree String -> ([(Text, Text)], Either Text Text)
resToCoffeeString elidedPrefixes _parent routePrefix (ResourceLeaf res) =
  ([], Right $ intercalate "\n" $ map mkLine jsName)
  where
    jsName =
      case resourceDispatch res of
        Subsite _ _ -> [] -- Silently ignore subsites.
        Methods _ _ ->
          -- Don't bother with methods, they get the same route anyway.
          -- Use "_" for the root.
          let resName_ = DT.replace "." "" $ DT.replace "-" "_" fullName
              fullName = intercalate "_" [pack st :: Text | Static st <- resourcePieces res]
          in [if null resName_ then "_" else resName_]
    pieces = DT.splitOn "/" routeString
    variables = zip variableNames (filter isVariable pieces)
      where variableNames = DT.cons <$> ['a'..'z'] <*> ("" : variableNames)
    mkLine jsName = "  " <> jsName <> "("
      <> csvArgs variables
      <> "): string { "
      -- <> presenceChk
      <> "return this.root + " <> quote (routeStr variables variablePieces) <> "; }"
    routeStr vars ((Left p):rest) | null p    = routeStr vars rest
                                  | otherwise = "/" <> p <> routeStr vars rest
    routeStr (v:vars) ((Right _):rest) = "/' + " <> fst v <> ".toString() + '" <> routeStr vars rest
    routeStr [] [] = ""
    routeStr _ [] = error "extra vars!"
    routeStr [] _ = error "no more vars!"

    isVariable r = length r > 1 && DT.head r == ':'
    variablePieces = map (\p -> if isVariable p then Right p else Left p) pieces
    csvArgs :: [(Text, Text)] -> Text
    csvArgs = intercalate "," . map (\(var, typ) -> var <> typ)
    quote str = "'" <> str <> "'"
    routeString = DT.replace "//" "/" routePrefix <> renderRoutePieces (resourcePieces res)
-- This is here because in the Flow code, we dont refer to
-- PATHS.api.doc.foo but PATHS.doc.foobar.  So we can keep our route
-- organization in place but also leave Flow alone.
resToCoffeeString elidedPrefixes parent routePrefix (ResourceParent name _ pieces children) | name `elem` elidedPrefixes =
    (concatMap fst res, Left $ intercalate "\n" (map (either id id . snd) res))
  where
    fxn = resToCoffeeString elidedPrefixes parent (routePrefix <> "/" <> renderRoutePieces pieces <> "/")
    res = map fxn children
resToCoffeeString elidedPrefixes parent routePrefix (ResourceParent name _ pieces children) =
    ([linkFromParent], Left $ resourceClassDef)
  where
    parentMembers f =
      intercalate "\n  " $ map f $ concatMap fst childFlow
    memberInitFromParent (slot, klass) = "  this." <> slot <> " = new " <> klass <> "(root);"
    memberLinkFromParent (slot, klass) = "" <> slot <> ": " <> klass <> ";"
    linkFromParent = (pref, resourceClassName)
    resourceClassDef =
        "class " <> resourceClassName <> " {\n"
      <> intercalate "\n" childMembers <> "\n"
      <> parentMembers memberLinkFromParent <> "\n"
      <> "  root: string;\n"
      <> "  constructor(root: string) {\n"
      <> "    this.root = root;\n  "
      <> parentMembers memberInitFromParent
      <> "\n  }\n"
      <> "}\n\n"
      <> intercalate "\n" childClasses
    (childClasses, childMembers) = partitionEithers $ map snd childFlow
    jsName = maybe "" (<> "_") parent <> pref
    childFlow = flip map (filter hasMethods children) $ resToCoffeeString
                            elidedPrefixes
                            (Just jsName)
                            (routePrefix <> "/" <> renderRoutePieces pieces <> "/")
    pref = cleanName $ pack name
    resourceClassName = "PATHS_TYPE_" <> jsName

-- Silently ignore routes without methods.
hasMethods (ResourceLeaf res) = case resourceDispatch res of { Methods _ [] -> False; _ -> True}
hasMethods _                  = True

deriving instance (Show a) => Show (ResourceTree a)
deriving instance (Show a) => Show (FlatResource a)
