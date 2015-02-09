package mjis.eval

import scala.util.control.ControlThrowable

class ReturnControlThrowable(val instance: Instance) extends Throwable(null, null, false, false) with ControlThrowable
