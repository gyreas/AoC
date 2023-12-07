#!/usr/bin/env ruby

def twodigit s
  # TODO: use array
  digits = {
     "zero" => "0",
      "one" => "1",
      "two" => "2",
    "three" => "3",
     "four" => "4",
     "five" => "5",
      "six" => "6",
    "seven" => "7",
    "eight" => "8",
     "nine" => "9",
  }

  d1 = ""
  d2 = ""
  dword = ""
  s.each_char do |ch|
    ch =~ /\d/
    # puts "O> ch: #{ch}"
    if $~.nil? # is not a digit
      # puts "o> ch: #{ch}, dword: #{dword}"
      # puts "inc: #{digits.keys.include? dword}"

      (dword.concat ch.strip) =~ /(zero|one|two|three|four|five|six|seven|eight|nine)/
      tmp = $~.to_s

      if digits.keys.include? tmp
        digit = digits[dword]
        # puts "digit: #{digit}"

        if d1.empty? then d1 = digit else d2 = digit end
        # dword.clear
      end
    else
      if d1.empty? then d1 = ch  else  d2 = ch  end
      dword.clear
    end
  end

  if d2.empty? then d2 = d1 end
  digits = d1.concat(d2)

  return digits.to_i
end

def getsum file
  # f = File.open file, "r"

  # f.readlines.map do |line|
  f = "cone9\n1one92\n"
  #   two1nine
  #   eightwothree
  #   abcone2threexyz
  #   xtwone3four
  #   4nineeightseven2
  #   zoneight234
  #   7pqrstsixteen
  # "
  # f.lines.map do |line|
  #   worked = twodigit line.strip
  #   puts "#{line} => #{worked}"
  #   worked
  # end.inject do |sum, n|
  #   sum + n
  # end
  f.lines.each do |line|
    puts "#{line} => #{twodigit line}"
  end
end

s = ARGV[0]
(getsum s)
