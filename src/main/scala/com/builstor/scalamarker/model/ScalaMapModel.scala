/**
 *
 */
package com.builstor.scalamarker.model

import freemarker.template.TemplateHashModel
import freemarker.template.TemplateModel
import freemarker.template.ObjectWrapper
import freemarker.template.TemplateScalarModel

/**
 * @author shaneed
 *
 */
class ScalaMapModel(val map: scala.collection.Map[String, _], val wrapper: ObjectWrapper) extends TemplateHashModel with TemplateScalarModel {
  def isEmpty = (map == null || map.isEmpty)

  def get(key: String): TemplateModel = wrapper.wrap(map.getOrElse(key, null))
  
  def getAsString = if(map == null) null else map.toString
}