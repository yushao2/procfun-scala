package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s7 = singletonSet(7)
    val s8 = singletonSet(8)
    val s9 = singletonSet(9)
    val s10 = singletonSet(10)

  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `Filter Even`: Unit = {
    new TestSets {
      val s = union(union(union(union(union(union(union(union(union(s1, s2),s3),s4),s5),s6),s7),s8),s9),s10)
      val filtered = filter(s, x => (x%2 == 0))
      printSet(filtered)
      assert(contains(filtered, 2), "Even")
      assert(!contains(filtered, 3), "Odd")
    }
  }

  @Test def `Exists`: Unit = {
    new TestSets {
      val s = union(union(union(union(union(union(union(union(union(s1, s2),s3),s4),s5),s6),s7),s8),s9),s10)
      assert(!exists(s, x => x == 11))
    }
  }

  @Test def `Forall`: Unit = {
    new TestSets {
      val s = union(union(union(union(union(union(union(union(union(s1, s2),s3),s4),s5),s6),s7),s8),s9),s10)
      assert(forall(s, x => x > 0 && x < 11))
    }
  }

  @Test def `Map`: Unit = {
    new TestSets {
      val s = union(union(union(union(union(union(union(union(union(s1, s2),s3),s4),s5),s6),s7),s8),s9),s10)
      val mapped = map(s, x => 2*x)
      assert(forall(mapped, x => (x%2 == 0 && x > 1 && x < 21)))
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
