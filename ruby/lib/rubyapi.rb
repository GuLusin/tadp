def invariant(&invariant_block)

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


def crear_contexto_ejecucion(parametros_metodo, metodo, valor_retorno = nil)
  context = Class.new

  parametros_metodo.zip(metodo.parameters).each do |retorno, valor_parametro|
    # puts "nombre_parametro #{valor_parametro[1]} retorna #{retorno}"
    context.define_singleton_method valor_parametro[1] do
      retorno
    end
  end

  if valor_retorno
    context.define_singleton_method :result do
      valor_retorno
    end
  end

  context
end

# cambiar nombre por una mejor abstraccion
def before_and_after_each_call

  @metodos_redefinidos = []

  # FIXME funciona pero esta feo
  invariants = if instance_variables.include? :@invariants
                 instance_variable_get :@invariants
               else
                 []
               end

  define_singleton_method :method_added do |metodo_nuevo|
    unless @metodos_redefinidos.include? metodo_nuevo
      sym_aux_metodos = "#{metodo_nuevo}_aux".to_sym

      @metodos_redefinidos.append metodo_nuevo
      @metodos_redefinidos.append sym_aux_metodos
      alias_method(sym_aux_metodos, metodo_nuevo)

      pre_cond = instance_variable_get :@pre_condition if instance_variables.include? :@pre_condition
      post_cond = instance_variable_get :@post_condition if instance_variables.include? :@post_condition

      define_method metodo_nuevo do |*argumentos|

        if pre_cond
          context_pre = crear_contexto_ejecucion(argumentos, method(sym_aux_metodos))
          raise "Failed to meet preconditions" unless context_pre.instance_eval(&pre_cond) # se podria hacer instance_exec antes
        end

        # problema de evaluar en un conexto especifico:
        # en el proc enviado solo puede usar parametros de la funcion donde se lo invoca
        # y no de otros metodos
        # agregar todos los otros metodos al contexto o hacerlo lazy? si falla en el contexto probar en la clase -> si falla en la clase mezclar clase con contexto

        valor_de_retorno = send(sym_aux_metodos, *argumentos)

        if post_cond #TODO codigo repetido pre_cond
          context_post = crear_contexto_ejecucion(argumentos, method(sym_aux_metodos),valor_de_retorno)
          raise "Failed to meet postconditions" unless context_post.instance_eval(&post_cond)
        end

        invariants.each do |invariant|
          raise "Exception invariant estado invalido" unless instance_exec(&invariant) #method(sym_aux_metodos).owner
        end

        valor_de_retorno

      end

      @pre_condition = nil
      @post_condition = nil

    end
  end
end