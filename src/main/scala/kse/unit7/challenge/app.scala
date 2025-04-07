package kse.unit7.challenge

import kse.unit7.challenge.adt.*
import kse.unit7.challenge.model.*
import kse.unit7.challenge.services.*

object app:

  def getPostsViews(apiKey: ApiKey): Try[List[PostView]] =
    for
      user  <- getUserProfile(apiKey)
      posts <- getPosts(user.userId)
      views <- posts.foldLeft(Try(List.empty[PostView])) { (cSave, post) =>
        for
          saver <- cSave
          view  <- getPostView(post)
        yield saver :+ view
      }
    yield views

  def getPostsViewDesugared(apiKey: ApiKey): Try[List[PostView]] =
    getUserProfile(apiKey).flatMap { profile =>
      getPosts(profile.userId).flatMap { posts =>
        posts.foldLeft(Try(List.empty)) { (cSave, post) =>
          cSave.flatMap { saver =>
            getPostView(post).map { view =>
              saver :+ view
            }
          }
        }
      }
    }

  def getPostView(post: Post): Try[PostView] =
    for
      comments <- getComments(post.postId)
      likes    <- getLikes(post.postId)
      shares   <- getShares(post.postId)
    yield PostView(post, comments, likes, shares)

  def getPostViewDesugared(post: Post): Try[PostView] =
    getComments(post.postId).flatMap { comments =>
      getLikes(post.postId).flatMap { likes =>
        getShares(post.postId).map { shares =>
          PostView(post, comments, likes, shares)
        }
      }
    }
