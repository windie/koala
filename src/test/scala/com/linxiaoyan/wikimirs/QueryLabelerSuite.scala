package com.linxiaoyan.wikimirs

import org.scalatest._
import org.scalatest.matchers._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import com.typesafe.config.ConfigFactory
import java.io.File
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils

@RunWith(classOf[JUnitRunner])
class BDBLabelStoragerSuite extends FunSuite with BeforeAndAfter {

  before {
    Settings.set(
      ConfigFactory.parseString("label.dir=build/label_test"))
  }

  test("put1") {
    synchronized {
      BDBLabelStorager.put(new LabelUnit("test_query", "test_formula", "test_url"), 2)
      assertEquals(2, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula", "test_url")))

      BDBLabelStorager.put(new LabelUnit("test_query", "test_formula", "test_url"), 3)
      assertEquals(3, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula", "test_url")))

      assertEquals(-1, BDBLabelStorager.get(new LabelUnit("none_query", "test_formula", "test_url")))
      assertEquals(-1, BDBLabelStorager.get(new LabelUnit("test_query", "none_formula", "test_url")))
      assertEquals(-1, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula", "nono_url")))
    }
  }

  test("put2") {
    synchronized {
      BDBLabelStorager.put(new LabelUnit("test_query", "test_formula", "test_url"), 2)
      assertEquals(2, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula", "test_url")))

      BDBLabelStorager.put(new LabelUnit("test_query", "test_formula1", "test_url1"), 3)
      assertEquals(2, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula", "test_url")))
      assertEquals(3, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula1", "test_url1")))

      BDBLabelStorager.put(new LabelUnit("test_query1", "test_formula1", "test_url1"), 4)
      assertEquals(2, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula", "test_url")))
      assertEquals(3, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula1", "test_url1")))
      assertEquals(4, BDBLabelStorager.get(new LabelUnit("test_query1", "test_formula1", "test_url1")))
    }
  }

  test("delete") {
    synchronized {
      BDBLabelStorager.put(new LabelUnit("test_query", "test_formula", "test_url"), -1)
      assertEquals(-1, BDBLabelStorager.get(new LabelUnit("test_query", "test_formula", "test_url")))
    }
  }

  test("allQueires") {
    synchronized {
      BDBLabelStorager.put(new LabelUnit("test_query1", "test_formula", "test_url"), 1)
      BDBLabelStorager.put(new LabelUnit("test_query2", "test_formula", "test_url"), 2)
      BDBLabelStorager.put(new LabelUnit("test_query3", "test_formula", "test_url"), 3)
      assertEquals(Set("test_query", "test_query1", "test_query2", "test_query3"), BDBLabelStorager.allQueries.toSet)
    }
  }

  after {
    Settings.reset
    FileUtils.deleteDirectory(new File("build/label_test"))
  }
}