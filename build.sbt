name := "advanced-scala"

version := "1.0"

scalaVersion := "2.12.4"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-laws" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-kernel" % "1.0.1"

libraryDependencies += "org.typelevel" %% "dogs-core" % "0.6.10"


resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

scalacOptions += "-Ypartial-unification"
