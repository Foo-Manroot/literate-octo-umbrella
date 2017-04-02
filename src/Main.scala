/* -------------------------------------------------------- */
/* Funciones principales del juego                          */
/* -------------------------------------------------------- */
package main

import utils._
import malla.Malla


object Main {

  val msg_menú = "1) Realizar movimiento\n" +
                 "0) Salir\n"

  val msg_mov = "1) Izquierda"

  /**
   * Función inicial.
   *
   * Se encarga de llamar a todas las funciones necesarias para obtener los argumentos
   * e iniciar el bucle principal.
   */
  def main (args: Array[String]) {

    val malla: Malla = Utils.obtener_args (args)

    println ("Malla: " + malla.matriz (0))
    malla.imprimir_matriz ();

    menú (malla)
  }

  /**
   * Imprime el menú y ejecuta el código según la opción seleccionada.
   *
   * @param malla
   *          Objeto con la información del juego
   */
  def menú (malla: Malla): Unit = {

    print (msg_menú +
           "--> Introduzca la opción seleccionada: ")

    Utils.pedir_opción (0, 1) match {

      case 0 => println (" ---- FIN ---- "); sys.exit (0)
      case 1 => pedir_mov (malla)
    }

    menú (malla)
  }

  /**
   * Pide la información necesaria para realizar el movimiento y la devuelve
   *
   * @param malla
   *          Objeto con la información del juego
   *
   * @return
   *          Una tupla con la fila y la columna (en ese orden)
   */
  def pedir_mov (malla: Malla): (Int, Int) = {

    print ("--> Introduzca la pieza a mover:\n" +
            "\t-> Fila (entre 0 y " + malla.filas + "): ")

    val fila = Utils.pedir_opción (0, malla.filas)

    print ("\t-> Columna (entre 0 y " + malla.columnas + "): ")
    val col = Utils.pedir_opción (0, malla.columnas)

    (fila, col)
  }

}
