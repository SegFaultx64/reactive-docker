package com.kolor.docker.api.types

import com.kolor.docker.api.InvalidRepositoryTagFormatException

class RepositoryTag private[RepositoryTag] (val repo: String, val tag: Option[String]) extends DockerEntity {
  override def toString = s"$repo/${tag.getOrElse("latest")}"
  override def equals(o: Any) = o match {
    case tag:RepositoryTag => tag.repo.eq(repo) && tag.tag.eq(tag)
    case _ => false
  }
}

object RepositoryTag {
	  val pattern = """^([A-z0-9:/.-]+):?([A-z0-9:/.-]+)$""".r

	  object RepTag {
	    def unapply(repo: String): Option[(String, Option[String])] = {
	      val tag_index = repo.lastIndexOf(':')
	      val nonTag = repo.take(tag_index)
	      val slash_index = repo.indexOf('/')
	      if (tag_index > slash_index)
	          return Some((nonTag, Some(repo.drop(nonTag.size + 1))))
	      else
	          return Some(repo, None)

	      return None
	    }
	  }
	  val patternNone = """^(<none>):?(<none>)*$""".r

	  def apply(s: String): RepositoryTag = s match {
	    case RepTag(repo, Some(tag)) => new RepositoryTag(repo, Some(tag))
	    case RepTag(repo, None) => new RepositoryTag(repo, None)
	    case patternNone(_, _) => new RepositoryTag("none", Some("none"))	// there might be images with no tags (e.g. zombie images)
		case _ => throw InvalidRepositoryTagFormatException(s"$s is an invalid repository tag", s)
	  }
	  
	  def unapply(tag: RepositoryTag): Option[String] = {
	    Some(tag.toString)
	  }
	  
	  def create(repo: String, tag: Option[String] = None) = new RepositoryTag(repo, tag)
}