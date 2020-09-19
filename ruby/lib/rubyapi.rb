# FIXME: esta todo definido para Object, habria que probar definirlo en Module

def invariant(&invariant_block)
  instance_variables.include?(:@invariants) ? @invariants.append(invariant_block) : @invariants = [invariant_block]
  # TODO: probar esto
  # @invariants ||= []
  # @invariants.append invariant_block

  method_redefinition
end

def pre(&pre_bloc)
  @pre_condition = pre_bloc
  method_redefinition
end

def post(&post_block)
  @post_condition = post_block
  method_redefinition
end

private
def crear_contexto_ejecucion(parametros_metodo, metodo)
  context = clone
  parametros_metodo.zip(metodo.parameters).each do |retorno, valor_parametro|
    # puts "nombre_parametro #{valor_parametro[1]} retorna #{retorno}"
    context.define_singleton_method(valor_parametro[1]) { retorno }
  end

  context
end

private
def evaluar_condicion(condicion, metodo, argumentos, mensaje_fallo, valor_retorno = nil)
  if condicion
    context = crear_contexto_ejecucion(argumentos, method(metodo))
    raise mensaje_fallo unless context.instance_exec(valor_retorno, &condicion)
  end
end

private
def method_redefinition
  @metodos_redefinidos = []
  invariants = @invariants # FIXME: no anda con el @invariants :(

  define_singleton_method :method_added do |metodo_nuevo|
    if @metodos_redefinidos && !@metodos_redefinidos.include?(metodo_nuevo)
      sym_aux_metodos = "__#{metodo_nuevo}_aux".to_sym

      @metodos_redefinidos.append metodo_nuevo
      @metodos_redefinidos.append sym_aux_metodos
      alias_method(sym_aux_metodos, metodo_nuevo) # TODO: se podria borrar el metodo auxiliar para mantener limpio el objeto
      # TODO to proc

      pre_cond = instance_variable_get :@pre_condition if instance_variables.include? :@pre_condition
      post_cond = instance_variable_get :@post_condition if instance_variables.include? :@post_condition

      # puts pre_cond.inspect

      define_method metodo_nuevo do |*argumentos|
        evaluar_condicion(pre_cond, sym_aux_metodos, argumentos, 'Failed to meet preconditions')

        valor_de_retorno = send(sym_aux_metodos, *argumentos)

        evaluar_condicion(post_cond, sym_aux_metodos, argumentos, 'Failed to meet postconditions', valor_de_retorno)

        # FIXME: no anda con el @invariants :(
        invariants&.each do |invariant|
          raise 'Exception invariant estado invalido' unless instance_exec(&invariant)
        end

        valor_de_retorno
      end

      @pre_condition = nil
      @post_condition = nil

    end
  end
end
