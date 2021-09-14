package recfun

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CalculationSuite extends AnyFunSuite {

  import Main.calculateFunction5

  val errorMessage: String = "Invalid input"
  test("When x>10 returns sum from 1 to 8 as k => kx") {
    assert(calculateFunction5(11) == 396.0)
  }

  test("When x<10 and k is given returns k*x**k") {
    assert(calculateFunction5(9, 2) == 162.0)
  }

  test("When x<10 and k is NOT given throws error") {
    try {
      calculateFunction5(9)
      assert(false)
    } catch {
      case error: Error => assert(error.getMessage == errorMessage)
    }
  }


  test("When x>10 and k IS given throws error") {
    try {
      calculateFunction5(11, 5)
      assert(false)
    } catch {
      case error: Error => assert(error.getMessage == errorMessage)
    }
  }


  test("When x == 10 throws error") {
    try {
      calculateFunction5(10)
      calculateFunction5(10, 5)
      assert(false)
    } catch {
      case error: Error => assert(error.getMessage == errorMessage)
    }
  }


}
