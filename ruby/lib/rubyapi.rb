def invariant(&invariant_block)

  #TODO el if de abajo se deberia poder reemplazar por esto, pero falla
  #instance_variables.include? :@invariants ? @invariants.append invariant_block : @invariants = [invariant_block]

  if instance_variables.include? :@invariants
    @invariants.append invariant_block
  else
    @invariants = [invariant_block]
  end

  before_and_after_each_call
end


def pre(&pre_bloc)
  @pre_condition = pre_bloc
  before_and_after_each_call
end

def post(&post_block)
  @post_condition = post_block
  before_and_after_each_call
end


def crear_contexto_ejecucion(parametros_metodo, metodo)

  context = clone

  parametros_metodo.zip(metodo.parameters).each do |retorno, valor_parametro|
    # puts "nombre_parametro #{valor_parametro[1]} retorna #{retorno}"
    context.define_singleton_method valor_parametro[1] do
      retorno
    end
  end

  context
end

# fixme cambiar nombre por una mejor abstraccion
def before_and_after_each_call

  @metodos_redefinidos = []

  define_singleton_method :method_added do |metodo_nuevo|
    if @metodos_redefinidos and !@metodos_redefinidos.include? metodo_nuevo
      sym_aux_metodos = "#{metodo_nuevo}_aux".to_sym

      @metodos_redefinidos.append metodo_nuevo
      @metodos_redefinidos.append sym_aux_metodos
      alias_method(sym_aux_metodos, metodo_nuevo) #todo se podria borrar el metodo auxiliar para mantener limpio el objeto

      pre_cond = instance_variable_get :@pre_condition if instance_variables.include? :@pre_condition
      post_cond = instance_variable_get :@post_condition if instance_variables.include? :@post_condition

      define_method metodo_nuevo do |*argumentos|

        if pre_cond #TODO codigo repetido
          context_pre = crear_contexto_ejecucion(argumentos, method(sym_aux_metodos))
          raise 'Failed to meet preconditions' unless context_pre.instance_eval(&pre_cond)
        end

        valor_de_retorno = send(sym_aux_metodos, *argumentos)

        if post_cond #TODO codigo repetido
          context_post = crear_contexto_ejecucion(argumentos, method(sym_aux_metodos))
          raise 'Failed to meet postconditions' unless context_post.instance_exec valor_de_retorno, &post_cond
        end

        @invariants&.each do |invariant|
          raise 'Exception invariant estado invalido' unless instance_exec(&invariant)
        end

        # Esto es lo mismo que lo de arriba
        # if @invariants
        #   @invariants.each do |invariant|
        #     raise "Exception invariant estado invalido" unless instance_exec(&invariant)
        #   end
        # end

        valor_de_retorno

      end

      @pre_condition = nil
      @post_condition = nil

    end
  end
end
