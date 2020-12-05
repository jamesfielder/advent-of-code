import fs2._

Stream(1, 2, 3).chunkN(2, allowFewer = false).take(2).map(_.last).collect{ case Some(i) => i}.toList