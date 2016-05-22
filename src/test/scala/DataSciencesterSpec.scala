package org.invisibletech.datasciencefromscratch

import DataSciencester._
import DataSciencesterTestDataFixture._

import org.scalatest.FunSpec

class DataSciencesterSpec extends FunSpec {

  describe("We should be able to get the expected statistics") {
    it("total number of connections") {
      assert(totalConnections((users, friendships)) == 24)
    }

    it("number of friends of one user") {
      val compiled: CompiledUsers = (users, friendships)

      assert(numberOfFriends(compiled.usersWithFriends(1)) == 3)
    }

    it("avg number of friends") {
      assert(averageNumberOfFriends((users, friendships)) == 2.4)
    }

    it("number of friends by id") {
      val friends = (users, friendships)
      assert(numberOfFriendsById(friends).size == 10)
      assert(numberOfFriendsById(friends) == List((0,2), (1,3), (2,3), (3,3), (4,2), (5,3), (6,2), (7,2), (8,3), (9,1)))
    }

    it("sorted number of friends by id") {
      val friends = numberOfFriendsById((users, friendships))
      assert(sortedNumberOfFriendsById(friends).size == 10)
      assert(sortedNumberOfFriendsById(friends) == List((1,3), (2,3), (3,3), (5,3), (8,3), (0,2), (4,2), (6,2), (7,2), (9,1)))
    }

    it("counts number of friends have in common with friends") {
      val friends = (users, friendships)
      assert(countOfCommonFoFs(friends.usersWithFriends(1), friends) == List((0,1), (2,2), (3,1)))
    }
  }
}

