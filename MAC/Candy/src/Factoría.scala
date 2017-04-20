/* ----------------------------------- */
/* Definiciones de métodos de factoría */
/* ----------------------------------- */
package utils


import partida.Partida


object Factoría {

  /**
   * Método de factoría para crear un objeto de tipo Malla en función del nivel
   * especificado.
   *
   * @param niv
   *            Nivel con el que crear el tablero
   *
   * @return
   *            Una nueva instancia de una malla, si se trata de un nivel aceptado.
   *          Si no, lanza una excepción.
   *
   * @throws
   *            Error -> si el nivel especificado no está reconocido.
   */
  def crear_partida (niv: Int): Partida = {

    niv match {

      case 0 => new Partida ( 3,  3, niv)
      case 1 => new Partida ( 7,  9, niv)
      case 2 => new Partida (11, 17, niv)
      case 3 => new Partida (15, 27, niv)
      case _ => throw new Error ("El nivel '" + niv + "' no está soportado")
    }
  }
}