/**
 *
 */
package com.builstor.scalamarker.model

import freemarker.template.ObjectWrapper
import freemarker.template.TemplateSequenceModel

/**
 * @author shaneed
 *
 */
class ScalaSequenceModel(val seq: Seq[_], val wrapper: ObjectWrapper) extends TemplateSequenceModel {
  def size = seq.size
  
  def get(idx: Int) = wrapper.wrap(seq(idx))
}