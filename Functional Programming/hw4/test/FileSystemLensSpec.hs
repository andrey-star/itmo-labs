module FileSystemLensSpec
  ( spec,
  )
where

import FileSystemLensTask (FS(..), contents, name, _Dir, _File)
import Test.Hspec (Spec, describe, it, shouldBe)
import Lens.Micro ((^.), (^..), traversed)

spec :: Spec
spec = do
  describe "lens" $ do
    it "name" $
      root ^. name `shouldBe` "root"
    it "contents" $
      a ^.. contents . traversed . name `shouldBe` ["a.txt", "b", "c.txt"]
    it "file prism" $
      a ^.. contents . traversed . _File . name `shouldBe` ["a.txt", "c.txt"]
    it "dir prism" $
      a ^.. contents . traversed . _Dir . name `shouldBe` ["b"]
  where
    root, a, b, c :: FS
    root =
      Dir
        { _name = "root",
          _contents = [a, b, c]
        }
    a =
      Dir
        { _name = "a",
          _contents = [aFile, b, cFile]
        }
    b =
      Dir
        { _name = "b",
          _contents = [bFile, cFile]
        }
    c =
      Dir
        { _name = "c",
          _contents = [cFile]
        }

    aFile =
      File
        { _name = "a.txt"
        }

    bFile =
      File
        { _name = "b.txt"
        }

    cFile =
      File
        { _name = "c.txt"
        }
