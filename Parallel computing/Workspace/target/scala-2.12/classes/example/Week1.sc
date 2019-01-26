private var uidCount = 0L
def getUniqueID: Long = {
  uidCount = uidCount+1
  uidCount
}

def startHilo() = {
  val t = new Thread {
    override def run() {
      val uids = for( i <- 0 until 10) yield getUniqueID()
      println(uids)
    }
  }
  t.start()
  t
}
