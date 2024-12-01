def temperatureDerating(data):
    if data['Code'] == "Other":
        if data['temperature'] > 50 and data['temperature'] <= 100:
            data['S']= data['S'] - (25-0)/(100-50)*(data['temperature'] - 50)*145.038
            data['U']= data['U'] - (25-0)/(100-50)*(data['temperature'] - 50)*145.038
        elif data['temperature'] > 100:
            data['S']= data['S'] - (25+ (68-25)/(200-100)*(data['temperature'] - 100))*145.038
            data['U']= data['U'] - (25+ (68-25)/(200-100)*(data['temperature'] - 100))*145.038
        
        return data
    if data['Code'] == "ASME B31.4":
        data['TemperatureDerating'] = 1
        return data

    if data['Code'] == "ASME B31.8":
        data['TemperatureDerating'] = 1
        return data
