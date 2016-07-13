package examples

abstract class File

abstract class Executable

// programmer -> file
// programmingLanguage, file -> DockerBuild
//

class Programmer {
  def author(): File
}

class ProgrammingLanguage {
  def compile(file: File): Executable
}


