# -*- ruby -*-
if defined?(PryByebug)
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
end

# ===================
# Custom Pry aliases
# ===================

# Display execution timing
def time(&b)
  require 'benchmark'
  res = nil
  timing = Benchmark.measure do
    res = yield
  end
  puts "Using yield, it took:     user       system     total       real"
  puts "                      #{timing}"
  res
end
