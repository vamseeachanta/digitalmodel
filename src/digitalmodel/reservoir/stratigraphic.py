import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as colors

#Provide the UWI in the order that you want the Cross Section Displayed
wells_list=[WELL1,WELL2,WELL3,WELL4]
well_data_df=df_logs[df_logs['UWI'].isin(wells_list)]

# Convert 'Well' column to a categorical type with the specified order
well_data_df['UWI'] = pd.Categorical(well_data_df['UWI'], categories=wells_list, ordered=True)
well_data_df = well_data_df.sort_values(by=['UWI','Depth']).reset_index(drop=True)

#Create Tops Dataframe and Compute Thickness
tops_df=statdata[statdata['UWI'].isin(wells_list)]
tops_df['Thick']=tops_df['BASE']-tops_df['TOP']
# Group tops by well for easier access
well_tops = tops_df.set_index("UWI")[["TOP", "BASE"]].to_dict(orient='index')

# Determine depth limits based on tops data
depth_min = tops_df["TOP"].min()
depth_max = tops_df["BASE"].max()
# Group tops by well for easier access
well_thick = tops_df.groupby('UWI')["Thick"].apply(list).to_dict()
# Group the well data by WellName
wells = well_data_df.groupby("UWI")

tops_and_bases = []
# This Assumes if a Facies log (KMeans in this example) is present.
# Define colors for FACIES values
facies_colors = {
    0: 'm',
    1: 'blue',
    2: 'red',
    3: 'green',
    4: 'yellow',
    5: 'black'
}
# Create a list of colors in the order of FACIES values
color_list = [facies_colors[i] for i in sorted(facies_colors.keys())]
cmap_facies = colors.ListedColormap(color_list)

# Create a figure with 3 tracks for each well
fig, axes = plt.subplots(nrows=1, ncols=4 * len(wells), figsize=(18, 10),gridspec_kw={'hspace': 0, 'wspace': 0,'width_ratios': [1,0.8,0.8,0.6]*len(wells)}, sharey=True)

# Loop through each well group to plot logs
for i, (well_name, df) in enumerate(wells):
    # Get the top and base depths for alignment
    well_info = well_tops.get(well_name)
    top_depth = well_info['TOP']
    base_depth = well_info['BASE']

    # Store top and base for connecting lines
    tops_and_bases.append((well_name, top_depth, base_depth, i))

    # Adjust depths to align at the tops
    df['Adj_Depth']= df['Depth'] - top_depth  # Subtract top depth to align

    # Track indexes for each log type: GR, RHOB/NPHI, RT ad FACIES
    track_gr = i * 4  # Track 1 for Gamma Ray
    track_rt = i * 4 + 1  # Track 2 for Resistivity
    track_rhob_nphi = i * 4 + 2  # Track 3 for RHOB and NPHI
    track_facies = i * 4 + 3  # Track 4 for FACIES

    # Plot GR in the first track against depth
    axes[track_gr].plot(df['GR'], df['Adj_Depth'], color='green', label='GR')
    # Shade the GR track based on the conditions
    axes[track_gr].fill_betweenx(df['Adj_Depth'],df['GR'], 30, where=(df['GR'] < 30),
                     color='yellow', alpha=0.5, )  # Fill yellow for GR < 30
    axes[track_gr].fill_betweenx(df['Adj_Depth'], 30, df['GR'], where=(df['GR'] > 30),
                     color='grey', alpha=0.5, )  # Fill grey for GR > 30
    axes[track_gr].invert_yaxis()  # Depth increases downwards
    axes[track_gr].set_title(f"{well_name}")
    axes[track_gr].set_xlim(0, 75)
    axes[track_gr].legend(loc="upper right")

    # Plot RT in the second track and Fill the Resistivity track with a color gradient
    norm = plt.Normalize(df['RT'].min(), df['RT'].max())  # Normalize RT values for colormap
    colors_rt = plt.cm.rainbow(norm(df['RT']))  # Use the viridis colormap for RT values
    axes[track_rt].plot(df['RT'], df['Adj_Depth'], color='black', label='RT')
   



 # Fill between 0 and RT curve with the corresponding colors
    for j in range(len(df) - 1):  # Iterate through each point in RT
        axes[track_rt].fill_betweenx( df['Adj_Depth'].iloc[j:j + 2],
                                     0,
                                     df['RT'].iloc[j:j + 2],
                                     color=colors_rt[j],
                                     edgecolor='none')
    axes[track_rt].invert_yaxis()  # Invert the y-axis
    axes[track_rt].set_xlim(1, 1000)
    axes[track_rt].set_xscale('log')
    axes[track_rt].legend(loc="upper right")

    # Normalize RHOB and NPHI scales to fill between
    df['DPHI'] = (2.71-df['RHOB']) / (2.71 - 1)

    # Plot RHOB in the Third track against depth
    secax_rhob = axes[track_rhob_nphi].twiny()
    secax_rhob.plot(df['RHOB'], df['Adj_Depth'], color='red', label='RHOB')
    secax_rhob.invert_yaxis()
    secax_rhob.set_xlim(1.95, 2.95)  # Set RHOB x-axis limits
    secax_rhob.legend(loc="upper right")

    # Add a secondary x-axis for NPHI with its own limits
    secax_nphi = axes[track_rhob_nphi].twiny()
    # Plot NPHI on the same axis as RHOB (using the secondary x-axis)
    secax_nphi.plot(df['PHIN'], df['Adj_Depth'], color='blue', label='NPHI')
    secax_nphi.set_xlim(0.45, -0.15)  # Set NPHI scale
    secax_nphi.legend(loc="upper left")

    secax_nphi.fill_betweenx(
        df['Adj_Depth'], df['DPHI'], df['PHIN'],
        where=(df['DPHI']> df['PHIN']),
        color="yellow", alpha=0.7)
    secax_nphi.fill_betweenx(
        df['Adj_Depth'], df['DPHI'], df['PHIN'],
        where=(df['DPHI']< df['PHIN']),
        color="grey", alpha=0.7 )

    facies_data = df[['KMeans']].values
    axes[track_facies].imshow(facies_data, aspect='auto', cmap=cmap_facies,
                               extent=[-1, 1,df['Adj_Depth'].max(), df['Adj_Depth'].min()])
    axes[track_facies].set_xlabel("FACIES")

    #Remove scales from all tracks
    for ax in [axes[track_gr],axes[track_rhob_nphi], secax_rhob, secax_nphi,axes[track_rt], axes[track_facies]]:
        ax.set_xticks([])  # Remove x ticks
        ax.set_xlabel("")
    


# Connect the base of the 3rd track of the current well to the base of the 1st track of the next well
    for i in range(len(tops_and_bases) - 1):
        current_well_name, _, current_base_depth, _ = tops_and_bases[i]
        next_well_name, _, next_base_depth, _ = tops_and_bases[i + 1]

        # Adjusted depth values
        adjusted_current_base =current_base_depth - \
                                tops_df[tops_df['UWI'] == current_well_name]['TOP'].values[0]
        adjusted_next_base = next_base_depth - tops_df[tops_df['UWI'] == next_well_name]['TOP'].values[0]

        line_x = -1  # You can adjust this value for positioning the line
        axes[i * 4 + 3].plot([-1, 1], [adjusted_current_base, adjusted_next_base], color='purple',
                             linestyle='--', linewidth=2)
    if well_name in well_thick:
        for base in well_thick[well_name]:
            for ax in [axes[track_gr], axes[track_rt],axes[track_rhob_nphi]]:
                ax.axhline(y=base, color='purple', linestyle='--', linewidth=2)

# Draw a horizontal line across the top of the plot to "flatten" it
for ax in axes:
    ax.axhline(y=0, color='black', linewidth=1.5)
plt.tight_layout()
plt.show() 