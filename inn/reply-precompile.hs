sort [] = []
sort (x:xt) = sort (filter (<= x) xt) ++ [x] ++ sort (filter (> x) xt)

objectify source = do
  tab <- insert "#" neatPrim <$> singleFile source
  topo <- topoModules tab
  objs <- foldM compileModule Tip topo
  pure (fst <$> topo, objs)

dat n = putStr (show n) >> putChar ',' >> putChar ' '

main :: IO ()
main = do
  Right (topo, objs) <- objectify <$> getContents
  let
    ffiList = sort $ concatMap (keys . ffiImports . _neat) $ elems objs
    libFFI = fromList [("{foreign}", fromList $ zip ffiList [0..])]
    (libStart, lib) = foldl (genIndex objs) (0, libFFI) topo
  putStrLn "u precompiled_bytecode[] = {"
  dat $ fromIntegral $ length topo
  mapM (bytecodeDump lib) $ (objs !) <$> topo
  vmdump (topo, (\m -> m { _mem = [] }) <$> objs, ffiList)
  putStrLn "};"

bytecodeDump lib ob = do
  dat $ fromIntegral $ size $ _syms ob
  go lib $ elems $ _syms ob
  dat $ fromIntegral $ length $ _mem ob
  go lib $ _mem ob
  putChar '\n'
  where
  go lib = mapM \case
    Left (moduleName, sym) -> (if moduleName == "{foreign}" then dat else g) $ fromIntegral $ lib ! moduleName ! sym
    Right x -> dat $ fromIntegral x
  g n = dat $ 2*n + 128 + 1
