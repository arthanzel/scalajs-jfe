package org.scalajs.jfe

import org.eclipse.jdt.core.dom.InfixExpression.Operator
import org.eclipse.jdt.core.dom.InfixExpression.Operator._
import org.scalajs.ir.Trees.BinaryOp

object OperatorUtils {
  def binaryOpCode(resolvedTypeName: String, jdtOperator: Operator): BinaryOp.Code =
    (resolvedTypeName, jdtOperator) match {
      // TODO: String +

      case ("boolean", EQUALS) => BinaryOp.Boolean_==
      case ("boolean", NOT_EQUALS) => BinaryOp.Boolean_!=
    }
}
