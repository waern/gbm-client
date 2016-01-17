import Distribution.Simple
import Distribution.MacOSX

exclusions =
  ["/System/Library/Frameworks/",
   --"/libSystem.",
   "/libgcc_s.",
   "/libobjc."
  ]

app = MacApp {appName = "gbm-client", appIcon = Nothing, appPlist = Nothing, resources = [], otherBins = [], appDeps = ChaseWith exclusions}

main = defaultMainWithHooks simpleUserHooks {postBuild = appBundleBuildHook [app]}
