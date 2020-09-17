require '../lib/rubyapi'

class Operaciones
  #precondición de dividir
  pre { divisor != 0 }
  #postcondición de dividir
  post { result * divisor == dividendo } # fixme ? sintaxis modificada
  def dividir(dividendo, divisor)
    dividendo / divisor
  end
  # este método no se ve afectado por ninguna pre/post condición
  def restar(minuendo, sustraendo)
    minuendo - sustraendo
  end

end

# puts Operaciones.new.dividir(4, 2)
# puts Operaciones.new.dividir(4, 0)
