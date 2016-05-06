package org.invisibletech.datasciencefromscratch

import org.scalatest.FunSpec

class DataSciencesterSpec extends FunSpec {
  describe("Relations can be made bidirectional") {
    it("should show graph edge both ways"){
      val result = DataSciencester.createBiDirectionalRelations(List((1, 2)))  

      assert(result == List((1,2), (2, 1)))
    }
  }
}

