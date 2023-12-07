#!/usr/bin/env ruby

def twodigit s
  d1 = ""
  d2 = ""

  s.each_char do |ch|
    ch =~ /\d/
    # Is a digit?
    if ! $~.nil?
      if d1.empty? then d1 = ch else d2 = ch
      end
    end
  end
  if d2.empty? then d2 = d1 end

  digits = d1.concat(d2)

  return digits.to_i
end

def getsum file
  f = File.open file, "r"

  f.readlines.map do |line|
    twodigit line.strip
  end.inject do |sum, n|
    sum + n
  end
end

s = ARGV[0]
puts "Sum = #{getsum s}"
