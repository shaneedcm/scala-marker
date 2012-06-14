/**
 *
 */
package com.builstor.scalamarker.model

import freemarker.ext.beans.BeansWrapper
import freemarker.template.TemplateMethodModelEx
import java.lang.reflect.Method
import java.util.List
import freemarker.template.TemplateModel
import scala.collection.JavaConversions._

/**
 * @author shaneed
 *
 */
class ScalaOverloadedMethodModel(obj: Any, methods: Array[Method], wrapper: BeansWrapper) extends TemplateMethodModelEx {

  def exec(arguments: List[_]): Object = {
    val paramLength = arguments.size
    val arg = arguments.asInstanceOf[List[TemplateModel]]
    val potentialMethods = methods.view.filter(_.getParameterTypes.length == paramLength).iterator
    while (potentialMethods.hasNext) {
      val method = potentialMethods.next
      try {
        val typedArguments = getTypedArguments(arg, method.getParameterTypes)
        return method.invoke(obj, typedArguments: _*)
      }
      catch {
        case e =>
      }
    }
    null //if we can't find anything, return null
  }

  def getTypedArguments(arguments: List[TemplateModel], types: Array[Class[_]]): Array[Object] = {
    types.zip(arguments).map {
      _ match {
        case (clazz, model) => wrapper.unwrap(model, clazz)
      }
    }
  }

}