using System.Collections;
using UnityEngine;
using UnityEngine.UI;

public class WaveSpawner : MonoBehaviour
{
    public Transform spawnpoint;
    public WaveData[] wave;
    public int waveIndex;
    public static int enemies;
    public float timeBetweenWave;
    public Text countdownText;
    private float countdown;

    private void Start()
    {
        enemies = 0;
    }
    private void Update()
    {
        if (enemies > 0)
            return;

        if (Gamemanager.start && countdown <= 0)
        {
            StartCoroutine(SpawnWave());
            countdown = timeBetweenWave;
        }
        
        countdown -= Time.deltaTime;
        countdown = Mathf.Clamp(countdown, 0f, Mathf.Infinity);
        countdownText.text = string.Format("{0:00.00}", countdown);
    }
    IEnumerator SpawnWave()
    {
        WaveData waves = wave[waveIndex];
        for (int i = 0; i < waves.count; i++)
        {
            SpawnEnemy(waves.enemy);
            yield return new WaitForSeconds(1 / waves.rate);
        }
        waveIndex++;
        if (waveIndex == wave.Length)
        {
            Gamemanager.levelCompleted = true;
            this.enabled = false;
        }
    }
    public void SpawnEnemy(GameObject enemy)
    {
        Instantiate(enemy, spawnpoint.position, spawnpoint.rotation);
        enemies++;
    }
}
