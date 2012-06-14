package com.builstor.scalamarker.model

import freemarker.ext.beans.BeansWrapper
import freemarker.template.TemplateHashModel
import freemarker.template.TemplateModel
import freemarker.template.TemplateModelException
import freemarker.template.TemplateScalarModel
import java.lang.reflect.Method
import java.lang.reflect.Modifier

class ScalaObjectModel(obj: Any, wrapper: BeansWrapper) extends TemplateHashModel with TemplateScalarModel {
  def isEmpty = obj == null

  @throws(classOf[TemplateModelException])
  def get(methodName: String): TemplateModel = {
    val methods = getMethods(methodName)
    methods.length match {
      case 0 => wrapper.wrap(null)

      case 1 if methods(0).getParameterTypes.length == 0 => getValue(methods(0))

      case 1 => new ScalaSimpleMethodModel(obj, methods(0).getName, methods(0).getParameterTypes, wrapper)

      case other => new ScalaOverloadedMethodModel(obj, methods, wrapper)
    }
  }

  def getAsString = if (obj == null) null else obj.toString

  @throws(classOf[TemplateModelException])
  private[this] def getValue(method: Method): TemplateModel = {
    try {
      wrapper.wrap(method.invoke(obj))
    }
    catch {
      case e: Exception => throw new TemplateModelException("Exception while invoking method " + method.getName, e)
    }
  }

  private[this] def getMethods(methodName: String): Array[Method] = {
    obj.getClass.getMethods filter {
      m => m.getName.equals(methodName) && Modifier.isPublic(m.getModifiers)
    }
  }
}




/*
import freemarker.template.TemplateHashModel
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import freemarker.template.TemplateModel
import freemarker.template.ObjectWrapper
import freemarker.template.TemplateScalarModel
import freemarker.ext.beans.SimpleMethodModel
import freemarker.ext.beans.BeansWrapper
import freemarker.ext.beans.BeanModel

class ScalaObjectModel(val obj: scala.ScalaObject, val wrapper: BeansWrapper) extends TemplateHashModel with TemplateScalarModel {
  def isEmpty = obj == null

  def get(methodName: String): TemplateModel = {
    findMethod(methodName, obj.getClass) match {
      case None         => wrapper.wrap(null)

      case Some(method) => getModelFromMethod(method)
    }
  }
  
  def getAsString = if(obj == null) null else obj.toString

  private[this] def getModelFromMethod(method: Method): TemplateModel = {
    if (method.getParameterTypes.length == 0) { // no parameter - getter might be
      wrapper.wrap(method.invoke(obj))
    }
    else {
      new BeanModel(obj, wrapper).get(method.getName)
      //new ScalaMethodModel(obj, method.getName, wrapper)
    }
  }

  private[this] def findMethod(methodName: String, clazz: Class[_]): Option[Method] = {
    clazz.getDeclaredMethods.find(m => m.getName.equals(methodName) && Modifier.isPublic(m.getModifiers)) match {
      case None if clazz.getSuperclass != null => findMethod(methodName, clazz.getSuperclass)

      case other                               => other
    }
  }
}


class ScalaObjectModel(obj: Any, wrapper: BeansWrapper) extends BeanModel(obj, wrapper) {
  override def get(key: String): TemplateModel = {
    val model = super.get(key)
   println(key)
    model
  }
}*/