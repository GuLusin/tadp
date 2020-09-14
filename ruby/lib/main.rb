require './rubyapi'

class MiClase

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
    proc{ puts "Mensaje before para mensaje_1" },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts "Mensaje after para mensaje_1" }
  )

  def mensaje_1
    puts "exec mensaje_1"
    5
  end

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts "Mensaje before para mensaje_2" },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts "Mensaje after para mensaje_2" }
  )


  def mensaje_2
    puts "exec mensaje_2"
    3
  end

end

miclase = MiClase.new

puts miclase.mensaje_1
puts miclase.mensaje_2
