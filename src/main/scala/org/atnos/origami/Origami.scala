package org.atnos
package origami

import effect._

object Origami extends
  FoldMTypes with FoldMFunctions with FoldMImplicits with
  FoldableMFunctions with FoldableMImplicits with
  SafeTTypes with SafeTFunctions with SafeTImplicits with
  FoldId with
  FoldIO with
  FoldSafeT
