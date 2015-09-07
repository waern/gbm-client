import Distribution.Simple
import Distribution.MacOSX

app = MacApp {appName = "gbm-client", appIcon = Nothing, appPlist = Nothing, resources = [], otherBins = [], appDeps = ChaseWithDefaults}

main = defaultMainWithHooks simpleUserHooks {postBuild = appBundleBuildHook [app]}
