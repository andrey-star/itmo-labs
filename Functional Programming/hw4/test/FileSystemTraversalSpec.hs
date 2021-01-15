module FileSystemTraversalSpec
  ( spec,
  )
where

import FileSystemLensTask (FS(..))
import FileSystemTraversalTask (cd, file, ls)
import Test.Hspec (Spec, describe, it, shouldBe)
import Lens.Micro ((^..), (^?))

spec :: Spec
spec = do
  describe "prism" $ do
    it "ls" $
      root ^.. ls `shouldBe` ["a", "b", "c"]
    it "cd" $
      root ^.. cd "a" . ls `shouldBe` ["a.txt", "b", "c.txt"]
    it "file exists" $
      root ^? cd "a" . file "a.txt" `shouldBe` Just "a.txt"
    it "file doesn't exist" $
      root ^? cd "a" . file "b.txt" `shouldBe` Nothing
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
