package org.richardweiss.nmf

import metascala.Nats.Nat

trait Matrix[Rows <: Nat, Cols <: Nat] {
  type Transpose = Matrix[Cols, Rows]
  type Multiply[_ <: Matrix[Cols, A], A <: Nat] = Matrix[Rows, A]
  type SameSized[_ <: Matrix[Rows, Cols]]
  def transpose: Matrix[Cols, Rows] = Matrix.matrix[Cols, Rows]
  def multiply[A <: Nat](m: Matrix[Cols, A]): Matrix[Rows, A] = Matrix.matrix[Rows, A]
  def sameSized[M <: Matrix[Rows, Cols]](m: M): Null = null
  def multiplyAndPack[A <: Nat](m: Matrix[Cols, A], p: Matrix[Rows, A]): Null = null
}

object Matrix {
  def matrix[Rows <: Nat, Cols <: Nat]: Matrix[Rows, Cols] = new Matrix[Rows, Cols] {}
}