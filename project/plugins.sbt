logLevel := Level.Debug
logLevel := Level.Warn
//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.2")
resolvers in ThisBuild += "Artima Maven Repository" at "http://repo.artima.com/releases"
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.7")

