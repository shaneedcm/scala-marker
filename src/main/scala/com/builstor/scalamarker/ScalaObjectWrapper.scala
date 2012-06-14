package com.builstor.scalamarker

import freemarker.template.DefaultObjectWrapper
import freemarker.template.TemplateModel
import freemarker.template.TemplateModelException
import com.builstor.scalamarker.model.ScalaMapModel
import com.builstor.scalamarker.model.ScalaSequenceModel
import com.builstor.scalamarker.model.ScalaObjectModel
import scala.collection.JavaConversions
import scala.collection.JavaConversions.JListWrapper
import freemarker.template.TemplateSequenceModel
import freemarker.template.SimpleSequence
import freemarker.template.TemplateHashModel
import scala.collection.JavaConversions.MapWrapper
import scala.collection.JavaConversions.JMapWrapper

class ScalaObjectWrapper extends DefaultObjectWrapper {

  @throws(classOf[TemplateModelException])
  override def wrap(obj: Any): TemplateModel = {
    obj match {
      case map: scala.collection.Map[String, _] => new ScalaMapModel(map, this)

      case seq: scala.collection.Seq[_]         => new ScalaSequenceModel(seq, this)

      case scalaObj: scala.ScalaObject          => new ScalaObjectModel(scalaObj, this)

      case other                                => super.wrap(other);
    }
  }

  override def unwrap(model: TemplateModel, hint: Class[_]) = {
    //list
    if (model.isInstanceOf[TemplateSequenceModel] && hint.isAssignableFrom(classOf[JListWrapper[_]])) {
      val result = super.unwrap(model, classOf[java.util.List[_]])
      JavaConversions.JListWrapper(result.asInstanceOf[java.util.List[_]])
    }
    //if map
    else if (model.isInstanceOf[TemplateHashModel]) {
      val result = super.unwrap(model, classOf[java.util.Map[_, _]])
      val mutableMap = JavaConversions.mapAsScalaMap(result.asInstanceOf[java.util.Map[_, _]])
      if (hint.isAssignableFrom(classOf[JMapWrapper[_, _]]))
        mutableMap
      else if (hint.isAssignableFrom(classOf[scala.collection.immutable.Map[_, _]])) {
        scala.collection.immutable.Map.empty[Any, Any] ++ mutableMap
      }
      else {
        super.unwrap(model, hint)
      }
    }
    else {
      val result = super.unwrap(model, hint)

      result
    }

  }
}