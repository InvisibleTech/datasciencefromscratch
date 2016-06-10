package org.invisibletech.datasciencefromscratch

import DataSciencester._

import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter

class CompiledUserDirectorySpec extends FunSpec with BeforeAndAfter {
  var friendships: List[(Int, Int)] = _

  var users: List[User] = _

  before {
    friendships = List((0, 1))
    users = List(
      Map("id" -> 0, "name" -> "Hero"),
      Map("id" -> 1, "name" -> "Dunn"))
  }

  describe("Relations can be made bidirectional") {
    it("should show graph edge both ways") {
      val result = CompiledUserDirectory(users, friendships).createBiDirectionalRelations(List((1, 2)))

      assert(result == List((1, 2), (2, 1)))
    }
    it("will produce duplicates if 1 way relationships repeat relations") {
      val result = CompiledUserDirectory(users, friendships).createBiDirectionalRelations(List((1, 2), (2, 1)))

      assert(result == List((1, 2), (2, 1), (2, 1), (1, 2)))
    }
  }

  describe("We should be able to establish friendships immutably and with delayed evaluation") {
    it("should allow following friends from myself and back again") {
      val result = CompiledUserDirectory(users, friendships)

      assert(result.usersWithFriends.size == 2)

      val herosFriends: List[scala.Function0[CompiledUser]] = result.usersWithFriends(0).friends
      assert(herosFriends.size == 1)

      val dunnsFriends: List[scala.Function0[CompiledUser]] = result.usersWithFriends(1).friends
      assert(dunnsFriends.size == 1)
    }
  }

}