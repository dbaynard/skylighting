import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity (normal)
-- main = defaultMain
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \a fs -> do
      files <- matchFileGlob "../skylighting-core/xml/*.xml"
      let v = normal `fromFlagOrDefault` buildVerbosity fs
      rawSystemExit v "skylighting-extract" files
      preBuild simpleUserHooks a fs
  }
