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
        let res = (resToCoffeeString Nothing "" $ ResourceParent "paths" False [] hackedTree)
        in  "/* @flow */\n" <>
            either id id (snd res)
            <> "\nvar PATHS: PATHS_TYPE_paths = new PATHS_TYPE_paths("<>prefix<>");\n"

    -- route hackery..
    fullTree = resourcesApp :: [ResourceTree String]
    landingRoutes = flip filter fullTree $ \case
        ResourceParent _ _ _ _ -> False
        ResourceLeaf res -> not $ elem (resourceName res) ["AuthR", "StaticR"]

    parentName :: ResourceTree String -> String -> Bool
    parentName (ResourceParent n _ _ _) name = n == name
    parentName _ _  = False

    parents =
        -- if routePrefixes is empty, include all routes
        filter (\n -> routePrefixes == [] || any (parentName n) routePrefixes) fullTree
    hackedTree = ResourceParent "staticPages" False [] landingRoutes : parents
    cleanName = underscorize . uncapitalize . dropWhileEnd DC.isUpper
      where uncapitalize t = (toLower $ take 1 t) <> drop 1 t
            underscorize = DT.pack . go . DT.unpack
              where go (c:cs) | DC.isUpper c = '_' : DC.toLower c : go cs
                              | otherwise    =  c                 : go cs
                    go [] = []

    isNumberType "Int" = True
    isNumberType type_ = "Id" `isSuffixOf` type_ -- UserId, PageId, PostId, etc.

    renderRoutePieces pieces = intercalate "/" $ map renderRoutePiece pieces
    renderRoutePiece p = case p of
        Static st                          -> pack st :: Text
        Dynamic type_ | isNumberType type_ -> ": number"
                      | otherwise          -> ": string"
    isVariable r = length r > 1 && DT.head r == ':'
    resRoute res = renderRoutePieces $ resourcePieces res
    fullName res = intercalate "_" [pack st :: Text | Static st <- resourcePieces res]
    singleSlash = DT.replace "//" "/"
    resToCoffeeString :: Maybe Text -> Text -> ResourceTree String -> ([(Text, Text)], Either Text Text)
    resToCoffeeString _parent routePrefix (ResourceLeaf res) =
        -- previously assumed there weren't multiple methods per route path
        -- now hacking in support
        let jsNames = case resourceDispatch res of
                Subsite _ _ -> [] -- silently ignore subsites
                Methods _ [] -> error "no methods! (never here, check hasMethods)"
                Methods _ methods ->
                    let resName_ = DT.replace "." "" $ DT.replace "-" "_" $ fullName res
                        prefix_  = if resName_ == "" then "" else resName_ <> "_"
                        -- we basically never will want to refer to OPTIONS
                        -- routes directly
                        callableMeths = filter (\a -> a /= "OPTIONS") methods in
                    if length callableMeths > 1 || resName_ == ""
                        then map ((prefix_ <>) . toLower . pack) callableMeths
                        else [resName_]
        in ([], Right $ intercalate "\n" $ map mkLine jsNames)
      where
        pieces = DT.splitOn "/" routeString
        variables = snd $ foldl' (\(i,prev) typ -> (i+1, prev <> [("a" <> tshow i, typ)]))
                             (0::Int, [])
                             (filter isVariable  pieces)
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

        variablePieces = map (\p -> if isVariable p then Right p else Left p) pieces
        csvArgs :: [(Text, Text)] -> Text
        csvArgs = intercalate "," . map (\(var, typ) -> var <> typ)
        quote str = "'" <> str <> "'"
        routeString = singleSlash routePrefix <> resRoute res

    -- This is here because in the Flow code, we dont refer to
    -- PATHS.api.doc.foo but PATHS.doc.foobar.  So we can keep our route
    -- organization in place but also leave Flow alone.
    resToCoffeeString parent routePrefix (ResourceParent name _ pieces children) | name `elem` elidedPrefixes =
        (concatMap fst res, Left $ intercalate "\n" (map (either id id . snd) res))
      where
        fxn = resToCoffeeString parent (routePrefix <> "/" <> renderRoutePieces pieces <> "/")
        res = map fxn children

    resToCoffeeString parent routePrefix (ResourceParent name _ pieces children) =
        ([linkFromParent], Left $ resourceClassDef)
      where
        parentMembers f =
          intercalate "\n  " $ map f $ concatMap fst childFlow
        memberInitFromParent (slot, klass) = "  this." <> slot <> " = new " <> klass <> "(root);"
        memberLinkFromParent (slot, klass) = "" <> slot <> ": " <> klass <> ";"
        linkFromParent = (pref, resourceClassName)
        resourceClassDef = "class " <>  resourceClassName  <> " {\n"
          <> "  " <> "root: string;\n"
          <> intercalate "\n" childMembers
          <> "  " <> parentMembers memberLinkFromParent
          <> "\n"
          <> "  constructor(root: string){\n"
          <> "    this.root = root;\n  "
          <> parentMembers memberInitFromParent
          <> "\n  }\n"
          <> "}\n\n"
          <> intercalate "\n" childClasses
        (childClasses, childMembers) = partitionEithers $ map snd childFlow
        jsName = maybe "" (<> "_") parent <> pref
        childFlow = flip map (filter hasMethods children) $ resToCoffeeString
                                (Just jsName)
                                (routePrefix <> "/" <> renderRoutePieces pieces <> "/")
        pref = cleanName $ pack name
        resourceClassName = "PATHS_TYPE_" <> jsName
    -- Silently ignore routes without methods.
    hasMethods (ResourceLeaf res) = case resourceDispatch res of { Methods _ [] -> False; _ -> True}
    hasMethods _                  = True

deriving instance (Show a) => Show (ResourceTree a)
deriving instance (Show a) => Show (FlatResource a)
