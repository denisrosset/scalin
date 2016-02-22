package scalin

class RangeExtender(val re: Range) extends AnyVal {

  def getRangeWithoutNegativeIndexes(totalLength: Int): Range = {
    if(re.isInclusive) {
      // actualStart will be given as argument to inclusive range "to"
      val actualStart = if ( re.start < 0 ) totalLength + re.start else re.start
      // actualEnd will be given as argument to inclusive range "to"
      val actualEnd = if ( re.end < 0 ) totalLength + re.end else re.end  
      (actualStart to actualEnd by re.step)
    } else if (re.end < 0 || re.start < 0)
      throw new IllegalArgumentException("cannot use negative end indexing with 'until', due to ambiguities from Range.end being exclusive")
    else
      re
  }

}
