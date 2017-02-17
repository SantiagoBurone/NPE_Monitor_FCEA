#####################################################################################
###############Conversores de consultas a medida del SGB-UdelaR######################
#####################################################################################


#######Introduccion

Este repositorio guarda las sintaxis e instrucciones para la conversión, el compilado, y actualizado de las consultas a medidas extraídas del Sistema de Gestión de Bedelías de la Universidad de la República.

El procedimiento está programado en el software estadístico R*.

NINGUNA BASE DE DATOS SERÁ ALOJADA EN ESTE REPOSITORIO. El cometido del mismo es el de compartir y colaborar 
con el procedimiento utilizado para el compilado y armado de estas base de datos.

Por cualquier consulta dirigirse a msilva@ccee.edu.uy


*R Core Team (2016). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  URL https://www.R-project.org/.

#######Detalles

Para cada consulta a medida para la que se haya programado un script de conversión desde su formato crudo de extracción del sistema a un formato de base de datos convencional existe un script con su mismo nombre.
Por ejemplo, para convertir consultas extraídas mediante la consulta a medida 'g_datosgen' se debe utilizar el script 'g_datosgen.r'.

########Registro de cambios (changelog)

#17/2/2017 - v 1.0.0 Lanzada:
-Incluye conversor para 'g_actividades_de_una_gen'
-Incluye conversor para 'g_asig'
-Incluye conversor para 'g_carcic'
-Incluye conversor para 'g_datosgen'
-Incluye conversor para 'g_egre_anio'
-Incluye conversor para 'g_genactual'
-Incluye conversor para 'g_icarr4'
-Incluye conversor para 'g_inscriptos'

#####################################################################################
    Conversores de consultas a medida del SGB-UdelaR
    Copyright (C) 2017 Mathias Silva

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#####################################################################################
