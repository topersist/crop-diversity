function [map] = f_holdridge_back2map(vector_in,land,index)

temp_back_to_map = NaN(size(land));
temp_back_to_map(index) = vector_in;
map = single(temp_back_to_map);

end