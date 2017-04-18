/* ---------------------------------------------- */
/* Funciones relacionadas con la interfaz gráfica */
/* ---------------------------------------------- */

import utils._
import malla.Partida

import scala.swing._
import java.awt.{Graphics2D,Color}


/**
 * Elemento en el que se dibujará la matriz.
 *
 * @param tam_bloques
 *          Tamaño de los bloques a dibujar (altura y anchura, en ese orden)
 *
 * @param partida
 *          Objeto con la infomación de la partida
 */
class Matriz (val tam_bloques: (Int, Int), val partida: Partida) extends Component {

  /**
   * Método para actualizar los elementos de la interfaz
   */
  override def paintComponent (g: Graphics2D) {

    val dim = size

    g.setColor (Color.white);
    g.fillRect (0, 0, dim.width, dim.height);

    val rowWid = dim.height / tam_bloques._1
    val colWid = dim.width / tam_bloques._2
    val wid = rowWid min colWid

    val x0 = (dim.width - tam_bloques._2 * wid) / 2
    val y0 = (dim.height - tam_bloques._1 * wid) / 2

    llenar_matriz (g, x0, y0)
/*
    for (x <- 0 until blocks.width) {
      for (y <- 0 until blocks.height) {
	g.setColor(FloodIt.colorFor(blocks.get(Pos(x, y))))
	g.fillRect(x0 + x * wid, y0 + y * wid, wid, wid)
      }
    }
*/
  }

  /**
   * Llena una fila con los diamantes de la lista
   */
  def llenar_matriz (g: Graphics2D, x: Int, y: Int): Unit = {

    val dim = size

    if ((x <= (size.width - tam_bloques._1))
        || (y <= (size.height - tam_bloques._2))) {

      dibujar_bloque (g, (x, y), 1)
    }
  }

  /**
   * Dibuja el diamante en la posición especifcada.
   *
   * @param g
   *          Elemento de tipo Graphics2D para dibujar en la pantalla
   *
   * @param pos
   *          Posición (x, y) en la que dibujar el rectángulo de tamaño tam_bloques
   *
   * @param valor
   *          Valor del diamante a dibujar, para especificar su color
   */
  def dibujar_bloque (g: Graphics2D, pos: (Int, Int), valor: Int) = {

    g.setColor (color (valor))

    g.fillRect (pos._1 + tam_bloques._1
                , pos._2 + tam_bloques._2
                , tam_bloques._1
                , tam_bloques._2)
  }

  /**
   * Devuelve el color que corresponde con el diamante
   */
  def color (diamante: Int): Color = {

    diamante match {

      case 1 => Color.BLUE
      case 2 => Color.RED
      case 3 => Color.YELLOW
      case 4 => Color.GREEN
      case 5 => Color.CYAN
      case 6 => Color.MAGENTA
      case 7 => Color.PINK
      case 8 => Color.WHITE
      case _ => Color.BLACK
    }
  }

}

/* ------------------------------------------------------------- */

class GUI (partida: Partida) extends MainFrame {


  val canvas = new Matriz ((20, 20), partida)

  /* Establece los parámetros de la ventana */
  title = "Candy"
  preferredSize = new Dimension (320, 240)
  /* Centra la ventana en la pantalla */
  peer.setLocationRelativeTo (null)

  contents = new BorderPanel {

    border = Swing.MatteBorder (8, 8, 8, 8, Color.white)
    add (canvas, BorderPanel.Position.Center)

    val buttons = new BoxPanel (Orientation.Horizontal) {
      border = Swing.EmptyBorder(8, 0, 0, 0)
      background = Color.white
      contents += Swing.HGlue
//      for (i <- 0 until FloodIt.NumColors) {
//	val b = Button(" ") { colorClick(i) }
//	b.background = FloodIt.colorFor(i)
//	contents += b
//	}
      contents += Swing.HGlue
      contents += Swing.HGlue
      contents += Button("Salir") { System.exit(0) }
    }

    add (buttons, BorderPanel.Position.South)
  }

}

/* ------------------------------------------------------------- */

object GUI_main {

  def main (args: Array[String]) {

    val partida: Partida = Utils.obtener_args (args)
    val l = Utils.crear_lista (partida.filas * partida.columnas, partida.nivel)
    val ui: GUI = new GUI (partida)

    ui.visible = true
  }
}
