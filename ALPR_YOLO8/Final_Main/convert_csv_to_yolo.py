import os
import pandas as pd

def convert_csv_to_yolo(main_dir):
    print(f"Scanning {main_dir} for CSV files...")
    
    # os.walk digs through all subdirectories automatically
    for root, dirs, files in os.walk(main_dir):
        for file in files:
            if file.endswith('.csv'):
                csv_file = os.path.join(root, file)
                print(f"\n--> Found annotation file: {csv_file}")
                
                try:
                    df = pd.read_csv(csv_file)
                    count = 0
                    
                    for index, row in df.iterrows():
                        filename = row['filename']
                        img_width = row['width']
                        img_height = row['height']
                        
                        xmin = row['xmin']
                        ymin = row['ymin']
                        xmax = row['xmax']
                        ymax = row['ymax']
                        
                        # Calculate center points
                        x_center = (xmin + xmax) / 2.0
                        y_center = (ymin + ymax) / 2.0
                        
                        # Calculate widths and heights
                        box_width = xmax - xmin
                        box_height = ymax - ymin
                        
                        # Normalize values (0 to 1)
                        x_center /= img_width
                        y_center /= img_height
                        box_width /= img_width
                        box_height /= img_height
                        
                        class_id = 0 # 0 is our license_plate class
                        yolo_line = f"{class_id} {x_center:.6f} {y_center:.6f} {box_width:.6f} {box_height:.6f}\n"
                        
                        # Save the .txt file in the exact same directory where this CSV was found
                        txt_filename = os.path.splitext(filename)[0] + '.txt'
                        txt_filepath = os.path.join(root, txt_filename)
                        
                        with open(txt_filepath, 'a') as f:
                            f.write(yolo_line)
                            
                        count += 1
                        
                    print(f"Successfully generated {count} YOLO .txt files in this folder.")
                    
                except Exception as e:
                    print(f"Could not process {csv_file}. Error: {e}")

    print("\nConversion complete! Your dataset is ready.")

if __name__ == '__main__':
    # Using your exact Windows path
    convert_csv_to_yolo(r"C:\Users\Tyler\Documents\Schoolwork\IST 691\Final_Main")