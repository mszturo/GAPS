package bitbucket.org.mstr93.gaps.domain

sealed trait Bit {
  def unary_! : Bit
}

object Zero extends Bit {
  override def unary_! : Bit = One
}

object One extends Bit {
  override def unary_! : Bit = Zero
}
