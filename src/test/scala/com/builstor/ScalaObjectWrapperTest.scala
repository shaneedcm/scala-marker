/**
 *
 */
package com.builstor

import org.scalatest.FunSuite
import com.builstor.scalamarker.ScalaObjectWrapper
import freemarker.template.TemplateBooleanModel
import freemarker.template.SimpleScalar
import freemarker.template.SimpleNumber
import freemarker.template.SimpleSequence
import freemarker.template.SimpleHash
import freemarker.template.TemplateHashModel
import com.builstor.scalamarker.model.ScalaMapModel
import com.builstor.scalamarker.model.ScalaSequenceModel
import com.builstor.scalamarker.model.ScalaSequenceModel
import freemarker.ext.beans.BeanModel
import freemarker.ext.beans.StringModel
import freemarker.template.TemplateScalarModel
import freemarker.template.TemplateNumberModel
import freemarker.cache.StringTemplateLoader
import freemarker.template.Configuration
import java.io.StringWriter
import java.math.BigDecimal

/**
 * @author shaneed
 *
 */
class ScalaObjectWrapperTest extends FunSuite {
  test("functionality of DefaultObjectWrapper is not affected") {
    val wrapper = new ScalaObjectWrapper
    //boolean
    assert(wrapper.wrap(true) == TemplateBooleanModel.TRUE)
    assert(wrapper.wrap(false) == TemplateBooleanModel.FALSE)

    //string
    val str = "test string"
    val strWrap = wrapper.wrap(str)
    assert(strWrap.isInstanceOf[SimpleScalar])
    assert(strWrap.asInstanceOf[SimpleScalar].getAsString === str)

    //number
    val integer = 12
    val intWrap = wrapper.wrap(integer)
    assert(intWrap.isInstanceOf[SimpleNumber])
    assert(intWrap.asInstanceOf[SimpleNumber].getAsNumber === 12)

    val long = 12000L
    val longWrap = wrapper.wrap(long)
    assert(longWrap.isInstanceOf[SimpleNumber])
    assert(longWrap.asInstanceOf[SimpleNumber].getAsNumber === long)

    val float = 1234.1234F;
    val floatWrap = wrapper.wrap(float)
    assert(floatWrap.isInstanceOf[SimpleNumber])
    assert(floatWrap.asInstanceOf[SimpleNumber].getAsNumber === float)

    val double = 1234.1234D;
    val doubleWrap = wrapper.wrap(double)
    assert(doubleWrap.isInstanceOf[SimpleNumber])
    assert(doubleWrap.asInstanceOf[SimpleNumber].getAsNumber === double)

    //testing array
    val array = Array(1, 2, 3, 4)
    val arrayWrap = wrapper.wrap(array)
    assert(arrayWrap.isInstanceOf[SimpleSequence])
    assert(arrayWrap.asInstanceOf[SimpleSequence].size === array.length)
    for (i <- 0 until array.length) {
      assert(arrayWrap.asInstanceOf[SimpleSequence].get(i).asInstanceOf[SimpleNumber].getAsNumber === array(i))
    }

    //testing java.util.List 
    val javaList = new java.util.ArrayList[String]
    javaList.add("a"); javaList.add("b"); javaList.add("c")
    val javaListWrap = wrapper.wrap(javaList)
    assert(javaListWrap.isInstanceOf[SimpleSequence])
    assert(javaListWrap.asInstanceOf[SimpleSequence].size === javaList.size)
    for (i <- 0 until javaList.size) {
      assert(javaListWrap.asInstanceOf[SimpleSequence].get(i).asInstanceOf[SimpleScalar].getAsString === javaList.get(i))
    }

    //testing java.util.Map
    val javaMap = new java.util.HashMap[String, Long]
    javaMap.put("a", 1L); javaMap.put("b", 2L); javaMap.put("c", 3L)
    val javaMapWrap = wrapper.wrap(javaMap)
    assert(javaMapWrap.isInstanceOf[TemplateHashModel])
    assert(javaMapWrap.asInstanceOf[TemplateHashModel].isEmpty === false)
    import scala.collection.JavaConverters._
    import scala.collection.JavaConversions._
    for (key <- javaMap.keySet) {
      assert(javaMapWrap.asInstanceOf[TemplateHashModel].get(key).asInstanceOf[SimpleNumber].getAsNumber === javaMap.get(key))
    }
    assert(javaMapWrap.asInstanceOf[TemplateHashModel].get("non exixsting") === null)

    //java object
    val javaObject = new JavaObject
    val javaObjectWrap = wrapper.wrap(javaObject)
    val javaObjectHash = javaObjectWrap.asInstanceOf[BeanModel]
    assert(javaObjectHash.get("a").asInstanceOf[SimpleNumber].getAsNumber === javaObject.getA)
    assert(javaObjectHash.get("b").asInstanceOf[SimpleScalar].getAsString === javaObject.getB)
  }

  test("wrapping scala.collection.map") {
    val wrapper = new ScalaObjectWrapper;

    //immutable collection
    {
      val map = scala.collection.Map[String, Any]("a" -> "b", "b" -> 2)
      val mapWrap = wrapper.wrap(map)
      assert(mapWrap.isInstanceOf[ScalaMapModel])
      val mapWrapModel = mapWrap.asInstanceOf[TemplateHashModel]
      assert(mapWrapModel.isEmpty === false)
      assert(mapWrapModel.get("a").asInstanceOf[SimpleScalar].getAsString === map("a"))
      assert(mapWrapModel.get("b").asInstanceOf[SimpleNumber].getAsNumber === map("b"))
      assert(mapWrapModel.get("non existing") === null)
    };

    {
      val map = scala.collection.immutable.Map[String, Any]("a" -> "b", "b" -> 2)
      val mapWrap = wrapper.wrap(map)
      assert(mapWrap.isInstanceOf[ScalaMapModel])
      val mapWrapModel = mapWrap.asInstanceOf[TemplateHashModel]
      assert(mapWrapModel.isEmpty === false)
      assert(mapWrapModel.get("a").asInstanceOf[SimpleScalar].getAsString === map("a"))
      assert(mapWrapModel.get("b").asInstanceOf[SimpleNumber].getAsNumber === map("b"))
      assert(mapWrapModel.get("non existing") === null)
    };

    {
      val map = scala.collection.mutable.Map[String, Any]("a" -> "b", "b" -> 2)
      val mapWrap = wrapper.wrap(map)
      assert(mapWrap.isInstanceOf[ScalaMapModel])
      val mapWrapModel = mapWrap.asInstanceOf[TemplateHashModel]
      assert(mapWrapModel.isEmpty === false)
      assert(mapWrapModel.get("a").asInstanceOf[SimpleScalar].getAsString === map("a"))
      assert(mapWrapModel.get("b").asInstanceOf[SimpleNumber].getAsNumber === map("b"))
      assert(mapWrapModel.get("non existing") === null)
    }
  }

  test("wrapping scala Sequence") {
    val wrapper = new ScalaObjectWrapper;

    {
      val seq = Seq(1, 2, 3)

      val seqWrap = wrapper.wrap(seq)
      assert(seqWrap.isInstanceOf[ScalaSequenceModel])
      val seqWrapModel = seqWrap.asInstanceOf[ScalaSequenceModel]
      assert(seqWrapModel.size === seq.size)
      for (i <- 0 until seq.size) {
        assert(seqWrapModel.get(i).asInstanceOf[SimpleNumber].getAsNumber === seq(i))
      }
    }

    {
      val seq = Seq("1", "b", "dsfsd")

      val seqWrap = wrapper.wrap(seq)
      assert(seqWrap.isInstanceOf[ScalaSequenceModel])
      val seqWrapModel = seqWrap.asInstanceOf[ScalaSequenceModel]
      assert(seqWrapModel.size === seq.size)
      for (i <- 0 until seq.size) {
        assert(seqWrapModel.get(i).asInstanceOf[SimpleScalar].getAsString === seq(i))
      }
    }
  }

  test("wrapping scala object & class") {
    case class ScalaClass(val a: Int, val b: String) {
      def mulBy3(x: String) = x * 3
    }

    val wrapper = new ScalaObjectWrapper

    val scalaObject = ScalaClass(2, "String")

    val wrapped = wrapper.wrap(scalaObject)
    assert(wrapped.isInstanceOf[TemplateHashModel])
    assert(wrapped.isInstanceOf[TemplateScalarModel])
    val model = wrapped.asInstanceOf[TemplateHashModel]
    assert(model.get("a").asInstanceOf[TemplateNumberModel].getAsNumber === scalaObject.a)
    assert(model.get("b").asInstanceOf[TemplateScalarModel].getAsString === scalaObject.b)
  }

  test("simple & overloaded methods") {
    class ScalaClass {
      def funInt(x: Int) = x * x;

      def funLong(x: Long) = x * x;

      def funFloat(x: Float) = x * x;

      def funDouble(x: Float, y: Int, z: Long) = x * y * z;

      def overload(x: String) = x

      def overload(x: Int) = x * x;

      def listMethod(x: scala.collection.mutable.Seq[BigDecimal]) = x.headOption.getOrElse(0)
    }

    val cfg = new Configuration
    cfg.setObjectWrapper(new ScalaObjectWrapper)
    val templateLoader = new StringTemplateLoader
    templateLoader.putTemplate("singleIntArg", """hello ${user.funInt(6)}""")
    cfg.setTemplateLoader(templateLoader);

    //testing integer
    {
      val template = cfg.getTemplate("singleIntArg")
      val writer = new StringWriter
      template.process(Map("user" -> new ScalaClass), writer)
      assert(writer.toString === "hello 36")
    }

    //testing Long
    {
      templateLoader.putTemplate("singleLongArg", """hello ${user.funLong(7)}""")
      val template = cfg.getTemplate("singleLongArg")
      val writer = new StringWriter
      template.process(Map("user" -> new ScalaClass), writer)
      assert(writer.toString === "hello 49")
    }

    //testing Float
    {
      templateLoader.putTemplate("singleFloatArg", """hello ${user.funFloat(7.5)}""")
      val template = cfg.getTemplate("singleFloatArg")
      val writer = new StringWriter
      template.process(Map("user" -> new ScalaClass), writer)
      assert(writer.toString === ("hello " + 7.5 * 7.5))
    }

    //testing Double and multimple arg
    {
      templateLoader.putTemplate("doubleArg", """hello ${user.funDouble(7.8, 4, 3)}""")
      val template = cfg.getTemplate("doubleArg")
      val writer = new StringWriter
      template.process(Map("user" -> new ScalaClass), writer)
      assert(writer.toString === ("hello " + 7.8 * 4 * 3))
    }
    
    //testing method with list argument
    {
      templateLoader.putTemplate("listMethod", """hello ${user.listMethod([2, 3])}""")
      val template = cfg.getTemplate("listMethod")
      val writer = new StringWriter
      template.process(Map("user" -> new ScalaClass), writer)
      assert(writer.toString === ("hello " + 2))
    }

    //testing overloaded methods
    {
      templateLoader.putTemplate("overload1", """hello ${user.overload(4)}""")
      val template = cfg.getTemplate("overload1")
      val writer = new StringWriter
      template.process(Map("user" -> new ScalaClass), writer)
      assert(writer.toString === ("hello " + 4 * 4))
    }
    {
      templateLoader.putTemplate("overload2", """hello ${user.overload("sha")}""")
      val template = cfg.getTemplate("overload2")
      val writer = new StringWriter
      template.process(Map("user" -> new ScalaClass), writer)
      assert(writer.toString === ("hello sha"))
    }
  }
}