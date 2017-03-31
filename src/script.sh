#!/bin/bash

# Compilador a usar
CC=scalac

# Intérprete
INTERP=scala

# Inicializa las variables necesarias con sus valores por defecto
DIR="$PWD" 			# Directorio de compilación
CP=""				# Classpath
ARGS=""				# Argumentos para el archivo final
NOMBRE_MAIN="main.Main"		# Nombre de la clase principal
OUTDIR=class			# Carpeta en la que se generan los archivos


# Texto de ayuda del uso del script
AYUDA="
Script para la compilación y ejecución de un archivo Scala (adaptado de
https://github.com/Foo-Manroot/Script-JFlex-CUP).
Miguel García Martín (Foo-Manroot) <miguel.garciamartin@hotmail.com> - 2016
v. 1.1

Llamada correcta:
$0 [-opciones | --opciones] [-a | --args][argumentos]

Estando disponibles las siguientes opciones:
	-a
	--args
		 Establece las opciones que se pasarán al ejecutar el archivo resultante
		de la compilación, si es que se necesitan. Todo lo que venga después de
		-a (o --args) se tomará como argumentos y se pasarán directamente. Si
		son varios elementos, deben separarse con dos puntos, ':'

	-c
	--clean
		 Limpia los archivos creados por este archivo (la carpeta $OUTDIR)

	-d
	--dir
		 Indica el directorio del proyecto a compilar.

	-h
	--help
		  Muestra esta ayuda y termina la ejecución

	-m
	--main
		 Establece la clase principal a ser ejecutada. Por defecto, su valor es
		$NOMBRE_MAIN

	-p
	--classpath
		 Establece el classpath para que java pueda ejecutarla (necesario para
		CUP). Por defecto está vacío. Si se quieren poner varias rutas, deben
		separarse con dos puntos -> \"path1:path2:path3\".
"

# Opciones en formato corto y largo para getopt
OP_CORTAS=acd:hp:m:
OP_LARGAS=args,clean,dir:,help,classpath:,main:

# Función sin terminar para procesar las opciones a mano y malamente por si getopt falla
args_a_mano ()
{

	# Ya si eso algún día lo haré bien... (aunque se supone que la mayoría de los sistemas soportan getpot)
	echo -e "
		$0: Error - No se pueden obtener los argumentos ('getopt --test' falló, seguramente porque el sistema no es compatible).

		El script se puede seguir usando, pero con los valores por defecto.
		"
	exit -1;
}

# Comprueba los argumentos y establece las variables de manera acorde
comprobar_args ()
{
	# Comprueba que se puede usar getopt para obtener las opciones
	getopt --test > /dev/null
	if [[ $? != 4 ]]
	then
		args_a_mano "$@"
	fi

	# Guarda el resultado para manejar correctamente los errores
	salida=$(getopt --options $OP_CORTAS --longoptions $OP_LARGAS --name "$0" -- "$@")

	if [[ $? != 0 ]]; then

		# Getopt devolvió error (argumentos desconocidos o mal usados)
		exit -2
	fi
	eval set -- "$salida"

	# Bucle para evaluar todos los argumentos disponibles
	while true
	do
		case "$1" in
			-h|--help)
				# Muestra la ayuda y sale
				echo -e "$AYUDA"
				exit 1;;

			-a|--args)
				#  Avanza en los argumentos, se queda con el resto como
				# los argumentos para el archivo final y sale del bucle
				shift 2;
				IFS=':' read -r -a ARGS <<< "$@"
				
				break;;

			-c|--clean)
				echo -e "-> Borrando la carpeta $OUTDIR/\n"
				rm -rf "$OUTDIR"
				echo -e "Archivos borrados"
				exit 1;;

			-d|--dir)
				# Comprueba que exista el directorio
				if [ -d "$2" ]
				then
					# Elimina el carácter final, /, si existe
					DIR="${2%/}"
				else
					echo -e "$0: Error - El directorio $2 no existe.\n" >&2
					exit -1;
				fi

				shift 2;;

			-m|--main)
				NOMBRE_MAIN="$2"
				shift 2;;

			-p|--classpath)
				# Sustituye la ruta por defecto
				CP="$2"
				shift 2;;

			--)
				# Sale del bucle (ya ha acabado con los argumentos)
				shift
				break;;
		*)
			echo "$0: Error no identificado - $1" >&2
			exit -3;;
		esac
	done

	# Cambia al directorio seleccionado, si es necesario
	cd "$DIR"
}

# Crea los archivos .class (jvm bytecode) a partir de los archivos fuente
compilar ()
{
	echo -e "\n--------------"
	echo -e "Creando archivos .class..."

	# Crea el archivo .class (redirecciona stderr a stdout
	# y almacena la salida en una variable)
	salida=$("$CC" -cp "$CP" -d "$OUTDIR" $(find "$DIR" -name '*.scala') 2>&1)

	# Comprueba si hay errores
	if [[ "$salida" =~ .error. ]]
	then
		echo -e "Errores en el código: \n $salida"
		return -1;
	else
		echo -e "Compilación correcta \n $salida"
	fi

	return 0;
}

# Ejecuta el main
ejecutar_main ()
{
	cd "$OUTDIR"
	$INTERP $NOMBRE_MAIN
}

# Ejecuta las funciones principales para la compilación y ejecución (si se quiere) del analizador
main ()
{
	#  Comprueba la existencia de los directorios necesarios.
	# El árbol de directorios final debe quedar como sigue:
	#	$CWD
	#	|
	#	|-------class/
	#		|____ ... (archivos .class)
	if [ ! -d "$OUTDIR" ]
	then
		mkdir --parents "$OUTDIR" -v
	fi

	# Compila para crear los archivos .class
	if compilar
	then
		echo -e "--------------"
		echo -e "\nTareas terminadas\n"

		# Si se ha creado correctamente, intenta ejecutar el archivo
		echo "¿Ejecutar el programa? (introducir el número de la respuesta elegida)"
		select respuesta in "Sí" "No"
		do
			case $respuesta in
		       		"Sí" )
					# Si se ha especificado un archivo .cup, busca Parser.class
					# y lo ejecuta. Si no, busca el archivo generado con JFlex.
					if [ "$NOMBRE_MAIN" ]
					then
						ejecutar_main
					fi

					break;;

			        "No" )
					echo -e "Para ejecutar el archivo, use la orden '$INTERP [-cp class] $NOMBRE_MAIN'"
					exit;;
			esac
		done

	else
		echo -e "$0: Error al compilar.\n" >&2
	fi
}


# Primero comprueba los argumentos y luego compila y ejecuta el programa
comprobar_args "$@"
main
