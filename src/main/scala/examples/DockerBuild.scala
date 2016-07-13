package examples

abstract class Filesystem {
  def read(filename: String): String
  def write(filename: String, v: String): String
}

class MergedFS(private val filesystems: List[Filesystem]) extends Filesystem {
  def read(filename: String): String = null
  def write(filename: String, v: String): String = null
}

class DockerBuild {
  def build(filesystems: List[Filesystem]): Filesystem  = {
    new MergedFS(filesystems)
  }
}

