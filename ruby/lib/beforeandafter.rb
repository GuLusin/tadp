require './rubyapi'

class MiClase

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
    proc{ puts 'Entré a un mensaje' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Salí de un mensaje' }
  )

  def mensaje_1
    puts 'mensaje_1'
    5
  end

  def mensaje_2
    puts 'mensaje_2'
    3
  end

end

miclase = MiClase.new
miclase.mensaje_2
miclase.mensaje_1
# Entré a un mensaje
# mensaje_2
# Salí de un mensaje
