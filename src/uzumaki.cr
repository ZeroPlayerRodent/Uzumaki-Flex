require "big"
macro length
    dirg.size - 1
end
macro cando(stuff)
    if queue.size > 0
        {{stuff}}
    end
end
macro move
    case dirg[y][x]
    when '6'; x+=1; dir = "right"
    when '4'; x-=1; dir = "left"
    when '2'; y-=1; dir = "up"
    when '8'; y+=1; dir = "down"
    when '0'; exit
    end
end
macro get_a
  if byte = STDIN.read_byte
    queue.insert(back, byte.to_big_i)
  else
    queue.insert(back, 0.to_big_i)
  end
end
macro get_i
    maybe_a_num = gets.not_nil!.chomp
    if maybe_a_num.index(/[^0-9]/)        
        queue.insert(back, 0.to_big_i)
    else
        queue.insert(back, maybe_a_num.to_big_i)
    end
end
macro jump(u, d, l, r)
    jumpin = true
    case dir
    when "up"; x+= {{u}}
    when "down"; x+= {{d}}
    when "left"; y+= {{l}}
    when "right"; y+= {{r}}
    end
    if dirg[y][x].in_set? "6428"
        puts "ERROR: Attempt to jump inward while in corner."
        exit
    end
    while dirg[y][x] == ' '
        case dir
        when "up"; x+= {{u}}
        when "down"; x+= {{d}}
        when "left"; y+= {{l}}
        when "right"; y+= {{r}}
        end
    end
end

macro bigjump
    jumpin = true
    case dir
    when "up"; x = 0
    when "down"; x = grid.size - 1
    when "left"; y = grid.size - 1
    when "right"; y = 0
    end
end

i = 0.to_big_i; x = i; y = i 
turn = i; dir = "right"; corner = false; offset = i;
printmode = false; acc = i; jumpin = false
queue = Array(BigInt).new; queue << i
code = File.read_lines ARGV[0]
grid = Array(Array(Char)).new
while i < code.size
  grid << code[i].chars
  i+=1
end
dirg = grid.clone
dirg[1][0] = '%'
while turn < dirg.size
    if x==offset&&y==offset+2
        dir="right"; turn+=1; corner=true
        offset+=2
    end
    if x==length-offset&&y==offset
        dir="down"; turn+=1; corner=true
    end
    if x==offset&&y==length-offset
        dir="up"; turn+=1; corner=true
    end
    if x==length-offset&&y==length-offset
        dir="left"; turn+=1; corner=true
    end
    case dir
    when "right"; dirg[y][x]='6';
        if corner==false; dirg[y+1][x] = ' '; end; x+=1; corner = false
    when "left"; dirg[y][x]='4';
        if corner==false; dirg[y-1][x] = ' '; end; x-=1; corner = false
    when "up"; dirg[y][x]='2'; 
        if corner==false; dirg[y][x+1] = ' '; end; y-=1; corner = false
    when "down"; dirg[y][x]='8' 
        if corner==false; dirg[y][x-1] = ' '; end; y+=1; corner = false
    end
end
case dir
when "right";x-=1
when "left";x+=1
when "up";y+=1
when "down";y-=1
end
dirg[y][x]='0'

y=0.to_big_i
x=0.to_big_i

back = -1
front = 0
while 0
    jumpin = false
    if grid[y].size != grid.size
        print "Not a perfect spiral!"
        exit
    end
    if !grid[y][x].in_set? "QIDPMOCAXSGWJKRHBVZVE#"
        puts "ERROR: Invalid Character '" + grid[y][x] + "'."
        exit
    end
    case grid[y][x]
    when 'E'; puts queue
    when '#'; printmode = true
    when 'Q'; queue.insert(back, 0.to_big_i)
    when 'I'; cando queue[front]+=1
    when 'D'; cando queue[front]-=1
    when 'P'; cando queue[front]+=10
    when 'M'; cando queue[front]-=10
    when 'O'; cando print queue[front]
    when 'C'; cando print (queue[front]%=255).to_i.chr
    when 'A'; cando acc = queue[front]
    when 'X'; cando queue.delete_at(front)
    when 'Z'; cando queue.insert(back, queue[front])
    when 'S'; get_a
    when 'V'; cando queue[front] += acc
    when 'G'; get_i
    when 'W'; bigjump
    when 'J'; if queue.size > 0 && queue[front]==acc; move; end
    when 'K'; if queue.size > 0 && queue[front]!=acc; move; end
    when 'R'; if front == 0; front = -1; back = 0; else;front = 0; back = -1; end
    when 'H'; jump -1, 1, 1, -1;
    when 'B'; jump 1, -1, -1, 1;
    end
    while printmode == true
        move
        if grid[y][x] != '#'
            print grid[y][x]
        end
        if grid[y][x] == '#'
            printmode = false
            break
        end
    end
    if jumpin == false
        move
    end
end
