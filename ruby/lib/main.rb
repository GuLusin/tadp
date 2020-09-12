#esto va en un archivo aparte pero nos falla ...
def before_and_after_each_call(pre_cond, post_cond) #TODO
  @metodos_redefinidos = []
  define_singleton_method :method_added do |metodo_nuevo|
    unless @metodos_redefinidos.include? metodo_nuevo
      @metodos_redefinidos.append metodo_nuevo
      sym_aux_metodos = "#{metodo_nuevo}_aux".to_sym
      @metodos_redefinidos.append sym_aux_metodos
      alias_method(sym_aux_metodos, metodo_nuevo)
      define_method metodo_nuevo do ||
        pre_cond.call
        send(sym_aux_metodos)
        post_cond.call
      end
    end
  end
end
#hasta aca

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
