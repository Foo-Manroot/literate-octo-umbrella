#!/bin/bash

COMPIL=scalac	# Compilador
INTERP=scala	# Intérprete

OUTDIR="class"	# Directorio en el que se crean los archivos .class


# Inicializa las variables necesarias con sus valores por defecto
DIR="$PWD" 			# Directorio de compilación
CP="$OUTDIR"			# Classpath
ARGS=""				# Argumentos para el archivo final

MAIN="Main"			# Nombre la clase principal
EXCLUDE_DIR="./lib"		# Directorios a excluir de la compilación



# Texto de ayuda del uso del script
AYUDA="
Script para la compilación y ejecución de un analizador con JFlex y CUP.
Miguel García Martín (Foo-Manroot) <miguel.garciamartin@hotmail.com> - 2016
v. 0.2

Llamada correcta:
$0 [-opciones | --opciones] [-a | --args][argumentos]

Estando disponibles las siguientes opciones:
	-a
	--args
		 Establece las opciones que se pasarán al ejecutar el archivo resultante
		de la compilación, si es que se necesitan. Todo lo que venga después
		de -a (o --args) se tomará como argumentos y se pasarán directamente.
		Si son varios elementos, deben separarse con dos puntos, ':'

	-d
	--dir
		 Indica el directorio en el que se encuentran los archivos .lex y .cup
		(si no se especifica nada, se toma por defecto el directorio actual).

	-e
	--exclude
		 Excluye el directorio para la compilación. Por defecto, es ./lib

	-h
	--help
		  Muestra esta ayuda y termina la ejecución

	-l
	--limpiar
	--clean
		 Elimina todos los archivos generados por el script (la carpeta $OUTDIR).

	-m
	--main
		 Especifica el nombre de la clase principal ($MAIN, por defecto).

	-p
	--classpath
		 Establece el classpath para que java pueda ejecutar los archivos de
		salida. Por defecto es \"$CP\". Si se quieren poner varias rutas,
		deben separarse con dos puntos -> \"path1:path2:path3\".
"

# Opciones en formato corto y largo para getopt
OP_CORTAS=ad:e:hlm:p:
OP_LARGAS=args,dir:,exclude:,help,limpiar,clean,main:,classpath:

# Función sin terminar para procesar las opciones a mano y malamente por si getopt falla
args_a_mano ()
{
	# Ya si eso algún día lo haré bien... (aunque se supone que la mayoría de los
	# sistemas soportan getpot)
	echo -e "
		$0: Error - No se pueden obtener los argumentos ('getopt --test' falló,"\
		" seguramente porque el sistema no es compatible).

		El script se puede seguir usando, pero con los valores por defecto.
		También se pueden cambiar estos valores directamente en el script.
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
	salida=$(getopt --options $OP_CORTAS --longoptions $OP_LARGAS \
		 --name "$0" -- "$@")

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

			"-d"|--dir)
				# Comprueba que exista el directorio
				if [ -d "$2" ]
				then
					# Elimina el carácter final, /, si existe
					DIR="${2%/}"
				else
					echo -e "$0: Error - El directorio '$2' no " \
						"existe." >&2
					exit -1;
				fi

				shift 2;;

			-e|--exclude)
				EXCLUDE_DIR="$2"

				shift 2;;

			-l|--clean|--limpiar)
				echo -e "-> Eliminando carpeta '$OUTDIR'"
				rm -rfv "$DIR/$OUTDIR"
				echo -e "-> Hecho <- "

				echo "Tareas terminadas."

				exit 1;;

			-m|--main)
				MAIN="$2"

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
			echo "$0: Error no identificado al interpretar los argumentos"\
				" - $1" >&2
			exit -3;;
		esac
	done

	# Cambia al directorio seleccionado, si es necesario
	cd "$DIR"
}

# Crea los archivos .class (jvm bytecode) a partir de los fuentes generados en /src
compilar ()
{
	echo -e "\n--------------"
	echo -e "Creando archivos .class...\n"

	# Crea el archivo .class (redirecciona stderr a stdout
	# y almacena la salida en una variable)
	salida=$("$COMPIL" -cp "$CP" -d "$OUTDIR" $(find . -name '*.scala'\
		-not -path "$EXCLUDE_DIR/*") 2>&1)

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

# Ejecuta el programa final
ejecutar ()
{
	RUTA=$(find . -name "$MAIN.class")

	# Cambia las barras por puntos para obtener la ruta y elimina el principio
	# (./class/) y la extensión (.class) para obtener el nombre de la clase
	RUTA="${RUTA#./class/}"
	RUTA="${RUTA%.class}"

	RUTA=$(echo -e "$RUTA" | sed -e 's/\//./g')

	if [ "$RUTA" ]
	then
		echo -e "\n--------------"
		echo -e "Ejecutando $RUTA"
                echo -e "--------------\n"

		"$INTERP" -cp "$CP" "$RUTA" ${ARGS[@]}
	else
		echo "$0: Error al intentar ejecutar $MAIN.class (no se ha encontrado)" >&2
	fi
}

# Ejecuta las funciones principales para la compilación y ejecución (si se quiere) del analizador
main ()
{
	#  Comprueba la existencia de los directorios necesarios.
	# El árbol de directorios final debe quedar como sigue:
	#	$CWD
	#	|-------class/
	#		|____ ... (archivos .class)
	if [ ! -d "$OUTDIR" ]
	then
		mkdir --parents "$OUTDIR" -v
	fi

	# Compila para crear los archivos .class
	if compilar
	then
		echo -e "\n--------------"
		echo -e "\nTareas terminadas\n"

		# Si se ha creado correctamente, intenta ejecutar el archivo
		echo "¿Ejecutar el programa? (introducir el número de la"\
		     " respuesta elegida)"
		select respuesta in "Sí" "No"
		do
			case $respuesta in
		       		"Sí" )
					# Si se ha especificado un archivo .cup,
					# busca Parser.class y lo ejecuta.
					# Si no, busca el archivo generado con
					# JFlex.
					if [ "$MAIN" ]
					then
						ejecutar
					fi

					break;;

			        "No" )
					echo -e "Para ejecutar el archivo, usar"\
						" la orden '$INTERP [-cp $CP]"\
						" $MAIN $ARGS'"
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
