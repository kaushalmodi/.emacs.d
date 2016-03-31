function dbhotlink(L)

   [ST, I] = dbstack('-completenames');
   if L+1 <=numel(ST)
      fprintf('<a href="matlab: opentoline(''%s'',%i,1)">%i </a>\n', ST(L+1).file, ...
              ST(L+1).line, ST(L+1).line);
   end
end
