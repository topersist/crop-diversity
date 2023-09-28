function holdridge_data_check = holdridge_data_check(step, year)
%%
old_data_folder = 'C:\Users\heikons1\OneDrive - Aalto University\Jatko-opinnot\Article 1\holdridge-main\holdridge_data\';
new_data_folder = 'C:\Users\heikons1\OneDrive - Aalto University\Jatko-opinnot\Article 1\scs\holdridge_data\';

if step == 1

    load(strcat(old_data_folder, 'holdridge_data_year', year, '_2.mat'), 'data_holdridge_future')
    future_data_old = data_holdridge_future;
    clearvars data_holdridge_future
    load(strcat(new_data_folder, 'holdridge_data_year', year, '_2.mat'), 'data_holdridge_future')
    future_data_new = data_holdridge_future;
    clearvars data_holdridge_future

    for i=1:8 %gcm`jjj
    %fprintf('GCM is')
    %i
        for j=1:3 %ssp
            %fprintf('SSP is')
            %j
            for k=1:4 %var
                %fprintf('var is')
                %k
                if j == 3
                    tf = isequal(future_data_old(:,:,k,j,i), future_data_new(:,:,k,j+1,i));
                else
                    tf = isequal(future_data_old(:,:,k,j,i), future_data_new(:,:,k,j,i));
                end
                
                if tf
                    %do nothing
                else
                  fprintf('Mistake here:')
                  disp(['GCM', num2str(i)]);
                  disp(['SSP', num2str(j)]);
                  disp(['var', num2str(k)]);
                end

            end
        end
    end

    elseif step == 2

        load(strcat(old_data_folder, 'holdridge_results_year', year, '_2.mat'), 'v_data_hold_future', 'v_data_hold_7_fut', 'hold_classes_fut')
        future_data_old = v_data_hold_future; 
        future_data7_old = v_data_hold_7_fut;
        future_data_class_old = hold_classes_fut;
        clearvars 'v_data_hold_future' 'v_data_hold_7_fut' 'hold_classes_fut'

        load(strcat(new_data_folder, 'holdridge_results_year', year, '_2.mat'), 'v_data_hold_future', 'v_data_hold_7_fut', 'hold_classes_fut')
        future_data_new = v_data_hold_future; 
        future_data7_new = v_data_hold_7_fut;
        future_data_class_new = hold_classes_fut;
        clearvars 'v_data_hold_future' 'v_data_hold_7_fut' 'hold_classes_fut'
  %%  
        for i=1:8 %gcm`jjj
        %fprintf('GCM is')
        %i
            for j=1:3 %ssp
                %fprintf('SSP is')
                %j
                for k=1:10 %column
                    %fprintf('col is')
                    %k
                    if j == 3
                        tf1 = isequal(future_data_old(:,k,j,i), future_data_new(:,k,j+1,i));
                        tf2 = isequal(future_data7_old(:,j,i), future_data7_new(:,j+1,i));
                        tf3 = isequal(future_data_class_old(:,:,j,i), future_data_class_new(:,:,j+1,i));

                    else
                        tf1 = isequal(future_data_old(:,k,j,i), future_data_new(:,k,j,i));
                        tf2 = isequal(future_data7_old(:,j,i), future_data7_new(:,j,i));
                        tf3 = isequal(future_data_class_old(:,:,j,i), future_data_class_new(:,:,j,i));
                    end
                    
                    if tf1 && tf2 && tf3
                        %do nothing
                    else
                      fprintf('Mistake here:')
                      tf1
                      tf2
                      tf3
                      disp(['GCM', num2str(i)]);
                      disp(['SSP', num2str(j)]);
                      disp(['col', num2str(k)]);
                    end
    
                end
            end
        end


    elseif step == 3

    else
        %do nothing
%%
end