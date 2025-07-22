import streamlit as st
import pandas as pd
import plotly.graph_objects as go

# Carregar os dados com lances
df = pd.read_csv("viagens_validas_com_lances.csv")

# Converter colunas de data/hora
df['Data'] = pd.to_datetime(df['Data'])
df['DATE.TIME..UTC.'] = pd.to_datetime(df['DATE.TIME..UTC.'])

# Calcular estatísticas por viagem (TRIP_ID)
viagens_info = df.groupby('TRIP_ID').agg(
    duracao=('Tempo_Relativo_Horas', 'max'),
    n_registos=('Tempo_Relativo_Horas', 'count'),
    vel_inicial=('Velocidade', lambda x: x.iloc[0]),
    vel_final=('Velocidade', lambda x: x.iloc[-1])
).reset_index()

# Aplicar os três critérios de filtragem
viagens_filtradas = viagens_info[
    (viagens_info['duracao'] > 2.5) &
    (viagens_info['duracao'] < 24) &
    ((viagens_info['n_registos'] / viagens_info['duracao']) >= 2) &
    (viagens_info['vel_inicial'] <= 0.5) &
    (viagens_info['vel_final'] <= 0.5)
]

# Filtrar o dataframe original
df = df[df['TRIP_ID'].isin(viagens_filtradas['TRIP_ID'])]

# Sidebar com filtros
st.sidebar.title("Filtros")

embarcacoes_disponiveis = sorted(df['Code'].unique())
embarcacao = st.sidebar.selectbox("Selecione uma embarcação", embarcacoes_disponiveis)

min_data = df['Data'].min().date()
max_data = df['Data'].max().date()
data_inicio, data_fim = st.sidebar.date_input("Intervalo de datas", [min_data, max_data])

# Filtro principal
df_filt = df[
    (df['Code'] == embarcacao) &
    (df['Data'] >= pd.to_datetime(data_inicio)) &
    (df['Data'] <= pd.to_datetime(data_fim))
]

# Eliminar viagens com apenas 1 registo
viagens_validas = df_filt['TRIP_ID'].value_counts()
viagens_validas = viagens_validas[viagens_validas > 1].index
df_filt = df_filt[df_filt['TRIP_ID'].isin(viagens_validas)]

viagens = sorted(df_filt['TRIP_ID'].unique())
st.sidebar.write(f"Viagens com >1 registo encontradas: {len(viagens)}")

# Exibição por viagem
for viagem in viagens:
    # Criar cópia para evitar SettingWithCopyWarning
    df_viagem = df_filt[df_filt['TRIP_ID'] == viagem].copy()

    data_viagem = df_viagem.iloc[0]['Data'].date()
    porto_partida = df_viagem.iloc[0]['which.PORT'] if pd.notna(df_viagem.iloc[0]['which.PORT']) else "Desconhecido"
    porto_chegada = df_viagem.iloc[-1]['which.PORT'] if pd.notna(df_viagem.iloc[-1]['which.PORT']) else "Desconhecido"

    with st.expander(f"{viagem}"):
        st.write(f"**Data da Viagem:** {data_viagem}")
        st.write(f"**Porto de Partida:** {porto_partida}")
        st.write(f"**Porto de Chegada:** {porto_chegada}")       
            
        # Definir as cores por ponto
        df_viagem['cor'] = df_viagem.apply(
            lambda row: 'red' if row['fase_extracao'] == 1 else ('blue' if row['inicio_lancamento'] == 1 else 'gray'),
            axis=1
        )
        
        # Mapear cores para lista (evita problemas no plotly)
        cor_map = {'red': 'red', 'blue': 'blue', 'gray': 'gray'}
        cores_lista = df_viagem['cor'].map(cor_map).tolist()
        
        
        # Criar figura com linha + pontos coloridos
        fig = go.Figure()

        fig.add_trace(go.Scatter(
            x=df_viagem['Tempo_Relativo_Horas'],
            y=df_viagem['Velocidade'],
            mode='lines',
            line=dict(color='lightgray'),
            name='Linha da Viagem',
            hovertemplate='Hora: %{x}<br>Velocidade: %{y} nós<extra></extra>'
        ))

        fig.add_trace(go.Scatter(
            x=df_viagem['Tempo_Relativo_Horas'],
            y=df_viagem['Velocidade'],
            mode='markers',
            marker=dict(color=cores_lista, size=7),
            name='Eventos',
            hovertemplate='Hora: %{x}<br>Velocidade: %{y} nós<br>Evento: %{marker.color}<extra></extra>'        
        ))

        fig.update_layout(
            #title="Lances durante a Viagem",
            title={
                'text': "Lances durante a Viagem",
                'font': {'size': 30}  
            },
            xaxis_title='Duração da Viagem (h)',
            yaxis_title='Velocidade (nós)',
            showlegend=False   
        )

        st.plotly_chart(fig, use_container_width=True, key=f"plot_{viagem}")

        # Resumo
        n_lances = df_viagem['lance_integrado'].sum()

        total_pontos = len(df_viagem)
        st.write("#### Resumo da Viagem")
        st.metric("Velocidade Média (nós)", round(df_viagem['Velocidade'].mean(), 2))
        st.metric("Duração da Viagem (h)", round(df_viagem['Tempo_Relativo_Horas'].max(), 2))
        st.metric("Número de Registos de Lance", int(n_lances))
        st.metric("Percentagem de Registos com Lance", f"{round(100 * n_lances / total_pontos, 1)} %")


# python -m streamlit run painel_lances.py