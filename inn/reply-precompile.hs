sort [] = []
sort (x:xt) = sort (filter (<= x) xt) ++ [x] ++ sort (filter (> x) xt)

objectify source = do
  tab <- insert "#" neatPrim <$> singleFile source
  topo <- topoModules tab
  objs <- foldM compileModule Tip topo
  pure (fst <$> topo, objs)

main :: IO ()
main = do
  Right showme@(topo, objs) <- objectify <$> getContents
  let
    ffiList = sort $ concatMap (keys . ffiImports . _neat) $ elems objs
    libFFI = fromList [("{foreign}", fromList $ zip ffiList [0..])]
-- TODO: seq without genIndex and libFFI ?
    (libStart, lib) = foldl (genIndex objs) (0, libFFI) topo

  mapM (scratchObj lib) $ (objs !) <$> topo
  vmdump showme
