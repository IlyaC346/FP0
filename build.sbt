ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "disk-analyzer",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ujson" % "3.3.1", // чтение JSON-конфигурации
    ),
    scalacOptions ++= Seq("-deprecation", "-feature"),
    // sbt assembly собирает один jar-файл со всеми зависимостями
    assembly / mainClass := Some("diskanalyzer.Main"),
    assembly / assemblyOutputPath := baseDirectory.value / "disk-analyzer.jar",
    // для запуска через "sbt run"
    run / fork := true,
    run / connectInput := true,
    outputStrategy := Some(StdoutOutput)
  )
