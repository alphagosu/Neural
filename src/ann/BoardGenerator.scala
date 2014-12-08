package ann
import chess._

class BoardGenerator {
  def rand(max: Int): Int = Math.floor( Math.random()*max ).toInt
  
  def findEmptySquare(pos: Position): Int = {
    var sq: Int = rand(63)
    while (pos.getPiece(sq) != Piece.EMPTY) sq = rand(63)
    sq
  }
  
  def yes: Boolean = rand(2) == 1
  
  def getBoard: (List[Long], Position) = { 
    val pos: Position = new Position()
    var isSet = false
    
    // Adding kings
    pos.setPiece(rand(64), Piece.WKING)
    pos.setPiece(findEmptySquare(pos), Piece.BKING)
    
    // Maybe add queens
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.WQUEEN)
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.BQUEEN)
    
    // Maybe add bishops    
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.WBISHOP)
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.WBISHOP)    
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.BBISHOP)
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.BBISHOP)
    
    // Maybe add knights    
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.WKNIGHT)
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.WKNIGHT)    
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.BKNIGHT)
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.BKNIGHT)
    
    // Maybe add rooks    
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.WROOK)
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.WROOK)    
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.BROOK)
    if (yes) pos.setPiece(findEmptySquare(pos), Piece.BROOK)
    
    // Maybe add pawns
    for( _ <- 1 to 8 ) {
      if (yes) pos.setPiece(findEmptySquare(pos), Piece.WPAWN)
      if (yes) pos.setPiece(findEmptySquare(pos), Piece.BPAWN)
    }
    
    // Is it black's turn?
    if (yes) pos.whiteMove = false
    
    // Castle masks?
    var castleMask = 0
    if (pos.getPiece(60) == Piece.WKING) {
      if (pos.getPiece(63) == Piece.WROOK && yes) castleMask |= (1 << Position.H1_CASTLE)
      if (pos.getPiece(56) == Piece.WROOK && yes) castleMask |= (1 << Position.A1_CASTLE)
    }
    if (pos.getPiece(4) == Piece.BKING) {
      if (pos.getPiece(7) == Piece.BROOK && yes) castleMask |= (1 << Position.H8_CASTLE)
      if (pos.getPiece(0) == Piece.BROOK && yes) castleMask |= (1 << Position.A8_CASTLE)
    }
    pos.setCastleMask(castleMask)
    
    // En passant?
    if (pos.whiteMove) {
      // Check for black pawns two spaces away from start
      val pawns = for (sq <- 24 to 31; if pos.getPiece(sq) == Piece.BPAWN) yield sq
      // Check for black pawns adjacent to white pawns
      val pawnsNeighbored = for (pawn <- pawns; if (pawn != 24 && pos.getPiece(pawn - 1) == Piece.WPAWN)
          || (pawn != 31 && pos.getPiece(pawn + 1) == Piece.WPAWN)) yield pawn
      // Check for clear space above black pawn, to ensure move could have happened
      val validPawns = for (pawn <- pawnsNeighbored; if pos.getPiece(pawn - 8) == Piece.EMPTY) yield pawn
      // If list not empty, choose one pawn randomly and, if yes, set en passant
      if (!validPawns.isEmpty && yes) pos.setEpSquare(validPawns(rand(validPawns.length)))
    } else {
      // Check for white pawns two spaces away from start
      val pawns = for (sq <- 32 to 39; if pos.getPiece(sq) == Piece.WPAWN) yield sq
      // Check for white pawns adjacent to black pawns
      val pawnsNeighbored = for (pawn <- pawns; if (pawn != 32 && pos.getPiece(pawn - 1) == Piece.BPAWN)
          || (pawn != 39 && pos.getPiece(pawn + 1) == Piece.BPAWN)) yield pawn
      // Check for clear space below white pawn, to ensure move could have happened
      val validPawns = for (pawn <- pawnsNeighbored; if pos.getPiece(pawn + 8) == Piece.EMPTY) yield pawn
      // If list not empty, choose one pawn randomly and, if yes, set en passant
      if (!validPawns.isEmpty && yes) pos.setEpSquare(validPawns(rand(validPawns.length)))
    }
    
    // Full move counter
    pos.fullMoveCounter = 1 + rand(49)
    
    //print(TextIO.asciiBoard(pos))
    
    (pos.pieceTypeBB.tail.toList, pos)
  }
}