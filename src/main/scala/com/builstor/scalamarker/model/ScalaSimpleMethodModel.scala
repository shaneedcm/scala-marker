/**
 *
 */
package com.builstor.scalamarker.model

import java.util.List
import freemarker.template.TemplateMethodModelEx
import freemarker.ext.beans.BeansWrapper
import java.lang.reflect.Method
import freemarker.template.TemplateModel
import scala.collection.JavaConversions._

/**
 * @author shaneed
 *
 */
class ScalaSimpleMethodModel(obj: Any, methodName: String, parameterTypes: Array[Class[_]], wrapper: BeansWrapper) extends TemplateMethodModelEx {

  override def exec(args: List[_]): Object = {
    val arguments = parameterTypes.zip(args.asInstanceOf[List[TemplateModel]]) map {
      _ match {
        case (clazz, model) => wrapper.unwrap(model, clazz)
      }
    }

    val method = obj.getClass.getMethod(methodName, parameterTypes: _*)
    method.invoke(obj, arguments: _*)
  }
}