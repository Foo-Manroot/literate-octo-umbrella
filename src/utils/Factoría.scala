/* ----------------------------------- */
/* Definiciones de métodos de factoría */
/* ----------------------------------- */
package utils


import malla.Malla


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
  def crear_malla (niv: Int): Malla = {

    niv match {

      case 1 => new Malla ( 7,  9, niv)
      case 2 => new Malla (11, 17, niv)
      case 3 => new Malla (15, 27, niv)
      case _ => throw new Error ("El nivel '" + niv + "' no está soportado")
    }
  }
}
