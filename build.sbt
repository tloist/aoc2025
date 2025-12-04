import Dependencies.*
val scala3Version = "3.7.4"
scalacOptions ++= (
  Seq(
    "-encoding", "UTF-8",
    "-source", "future",
    "-unchecked", "-deprecation"
  )
)

name := "Advent of Code 2025"

lazy val aoc2025 = project.in(file("."))
  .settings(
    name := "Advent of Code 2025 -- Root",
    version := "0.1.0",
    scalaVersion := scala3Version,
  )


lazy val day01 = dayProject( 1, "Secret Entrance")
lazy val day02 = dayProject( 2, "Gift Shop")
lazy val day03 = dayProject( 3, "Lobby")
lazy val day04 = dayProject( 4, "Printing Department")
  .dependsOn(map2d)

def dayProject(day: Int, title: String = ""): Project = Project.apply(f"day_$day%02d", file(f"days/$day%02d"))
  .settings(
    name := f"AoC Day $day%2d" + (if (title.nonEmpty) s" - $title" else ""),
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      osLib,
      cats,
      mUnit % Test,
    )
  )

lazy val map2d = Project("Map_2D", file("common/map2d"))
  .settings(
    name := "AoC Commons - 2D Map",
    version := "0.1.0",
    scalaVersion := scala3Version
  )